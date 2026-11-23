import csv
import os
import re
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np


BASE_DIR = Path(__file__).resolve().parents[3]
INPUT = BASE_DIR / "data/raw/project_raw" / "dce002.csv"
DEFINITIONS = (
    BASE_DIR
    / "data"
    / "semantic_scores"
    / "smeco2027_six_channels_transformer"
    / "smeco2027_six_channel_definitions.csv"
)
OUTPUT_DIR = BASE_DIR / "data/semantic_scores" / "smeco2027_six_channels_transformer"
WIDE_OUT = OUTPUT_DIR / "unesco_smeco2027_six_channel_scores_wide.csv"
LONG_OUT = OUTPUT_DIR / "unesco_smeco2027_six_channel_scores_long.csv"
COUNTRY_YEAR_OUT = OUTPUT_DIR / "unesco_smeco2027_six_channel_scores_country_year.csv"
REPORT_OUT = OUTPUT_DIR / "unesco_smeco2027_six_channel_scores_report.txt"

CHUNK_MAX_CHARS = 700
EMBEDDING_MODEL_NAME = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"

MISSING_TOKENS = {
    "",
    "no information",
    "no information.",
    "none",
    "n/a",
    "na",
    "not applicable",
    "not available",
}

PLACE_TERMS = {
    "abu dhabi",
    "africa",
    "african",
    "argentina",
    "argentine",
    "australia",
    "australian",
    "austria",
    "austrian",
    "brazil",
    "brazilian",
    "burkina",
    "canada",
    "canadian",
    "chile",
    "chilean",
    "colombia",
    "colombian",
    "cuba",
    "cuban",
    "cyprus",
    "dubai",
    "emirates",
    "ethiopia",
    "ethiopian",
    "eu",
    "europe",
    "european",
    "finland",
    "french",
    "germany",
    "ghana",
    "iceland",
    "icelandic",
    "india",
    "indian",
    "indonesia",
    "indonesian",
    "ireland",
    "irish",
    "italy",
    "kenya",
    "lithuania",
    "lithuanian",
    "mauritius",
    "mexico",
    "mexican",
    "norway",
    "norwegian",
    "palestine",
    "palestinian",
    "peru",
    "poland",
    "polish",
    "portugal",
    "serbia",
    "serbian",
    "sweden",
    "swedish",
    "tanzania",
    "uae",
    "uganda",
    "ukraine",
    "ukrainian",
    "uruguay",
    "viet nam",
    "vietnam",
    "zambia",
    "zealand",
    "zimbabwe",
}


def clean_value(value):
    text = (value or "").strip()
    text = re.sub(r"\s+", " ", text)
    if text.lower() in MISSING_TOKENS:
        return ""
    return text


def build_place_pattern(rows):
    terms = set(PLACE_TERMS)
    for row in rows:
        country = clean_value(row.get("Country Title EN"))
        if country:
            terms.add(country)
    terms = {term.strip() for term in terms if len(term.strip()) >= 3}
    terms = sorted(terms, key=len, reverse=True)
    return r"\b(?:" + "|".join(re.escape(term) for term in terms) + r")\b"


def remove_place_terms(text, place_pattern):
    if not place_pattern:
        return text
    return re.sub(place_pattern, " ", text, flags=re.IGNORECASE)


GENERIC_SENTENCE_PATTERNS = [
    r"^\s*this (policy|programme|program|measure|initiative)\b",
    r"^\s*the (policy|programme|program|measure|initiative)\b",
    r"\b(cultural and creative industries|creative industries|cultural sector)\b.{0,40}$",
]


def keep_substantive_sentences(text):
    sentences = re.split(r"(?<=[.!?])\s+", text)
    kept = []
    for sentence in sentences:
        sentence = sentence.strip()
        if not sentence:
            continue
        if len(sentence) < 35 and not re.search(
            r"\b(copyright|royalt|licens|brand|export|finance|loan|digital|platform|sustain|certif)",
            sentence,
            flags=re.IGNORECASE,
        ):
            continue
        if any(re.search(pattern, sentence, flags=re.IGNORECASE) for pattern in GENERIC_SENTENCE_PATTERNS):
            if not re.search(
                r"\b(copyright|royalt|licens|brand|export|market|finance|loan|credit|digital|platform|sustain|green|certif|training|rights|distribution|audience)",
                sentence,
                flags=re.IGNORECASE,
            ):
                continue
        kept.append(sentence)
    return " ".join(kept).strip()


def build_mechanism_document(row, place_pattern):
    content_parts = [
        clean_value(row.get("Goal")),
        clean_value(row.get("Main Feature")),
    ]
    doc = ". ".join(part for part in content_parts if part)
    doc = keep_substantive_sentences(doc)
    if not doc:
        fallback_parts = [
            clean_value(row.get("Title EN")),
            clean_value(row.get("Area of Monitoring")),
        ]
        doc = ". ".join(part for part in fallback_parts if part)
    doc = remove_place_terms(doc, place_pattern)
    return re.sub(r"\s+", " ", doc).strip()


def split_long_piece(piece, max_chars):
    words = piece.split()
    chunks = []
    current = []
    current_len = 0
    for word in words:
        extra_len = len(word) + (1 if current else 0)
        if current and current_len + extra_len > max_chars:
            chunks.append(" ".join(current))
            current = [word]
            current_len = len(word)
        else:
            current.append(word)
            current_len += extra_len
    if current:
        chunks.append(" ".join(current))
    return chunks


def split_document_chunks(text, max_chars=CHUNK_MAX_CHARS):
    if len(text) <= max_chars:
        return [text]
    pieces = re.split(r"(?<=[.!?])\s+", text)
    chunks = []
    current = ""
    for piece in pieces:
        piece = piece.strip()
        if not piece:
            continue
        if len(piece) > max_chars:
            if current:
                chunks.append(current)
                current = ""
            chunks.extend(split_long_piece(piece, max_chars))
            continue
        candidate = f"{current} {piece}".strip() if current else piece
        if len(candidate) <= max_chars:
            current = candidate
        else:
            chunks.append(current)
            current = piece
    if current:
        chunks.append(current)
    return chunks or [text]


def normalize_embeddings(values):
    norms = np.linalg.norm(values, axis=1, keepdims=True)
    norms[norms == 0] = 1
    return values / norms


def softmax(values, temperature=0.08):
    scaled = values / temperature
    scaled = scaled - np.max(scaled, axis=1, keepdims=True)
    exp_values = np.exp(scaled)
    return exp_values / exp_values.sum(axis=1, keepdims=True)


def score_to_0_1(cosine_scores):
    return np.clip((cosine_scores + 1.0) / 2.0, 0.0, 1.0)


def load_channels():
    with DEFINITIONS.open("r", encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))
    channels = []
    for row in rows:
        channels.append(
            {
                "channel": row["channel"],
                "label": row["channel_label"],
                "description": row["semantic_scoring_text"],
            }
        )
    return channels


def score_policy_chunks(model, docs, channel_texts):
    policy_chunks = []
    chunk_policy_indices = []
    chunk_weights = []
    for policy_index, doc in enumerate(docs):
        chunks = split_document_chunks(doc)
        for chunk in chunks:
            policy_chunks.append(chunk)
            chunk_policy_indices.append(policy_index)
            chunk_weights.append(max(len(chunk), 1))

    chunk_embeddings = normalize_embeddings(
        model.encode(policy_chunks, batch_size=32, show_progress_bar=True)
    )
    channel_embeddings = normalize_embeddings(model.encode(channel_texts, show_progress_bar=False))
    chunk_cosine_scores = chunk_embeddings @ channel_embeddings.T

    cosine_scores = np.zeros((len(docs), len(channel_texts)), dtype=np.float64)
    policy_weight_sums = np.zeros(len(docs), dtype=np.float64)
    for policy_index, weight, chunk_scores in zip(
        chunk_policy_indices, chunk_weights, chunk_cosine_scores
    ):
        cosine_scores[policy_index] += chunk_scores * weight
        policy_weight_sums[policy_index] += weight
    return cosine_scores / policy_weight_sums[:, None], len(policy_chunks)


def write_outputs(rows, docs, channels, cosine_scores, bounded_scores, relative_scores):
    channel_ids = [channel["channel"] for channel in channels]
    channel_labels = [channel["label"] for channel in channels]
    label_by_channel = dict(zip(channel_ids, channel_labels))

    with WIDE_OUT.open("w", encoding="utf-8", newline="") as f:
        base_fields = [
            "ID",
            "UUID",
            "Date",
            "ISO2 Code",
            "Country Title EN",
            "Regional Group",
            "Title EN",
            "top_mechanism_channel",
            "top_mechanism_label",
            "top_cosine_score",
            "top_relative_score",
            "second_mechanism_channel",
            "second_mechanism_label",
            "second_cosine_score",
            "document_text_chars",
        ]
        score_fields = [f"{channel}_cosine" for channel in channel_ids]
        bounded_fields = [f"{channel}_bounded" for channel in channel_ids]
        relative_fields = [f"{channel}_relative" for channel in channel_ids]
        writer = csv.DictWriter(
            f, fieldnames=base_fields + score_fields + bounded_fields + relative_fields
        )
        writer.writeheader()
        for i, (row, doc) in enumerate(zip(rows, docs)):
            order = np.argsort(cosine_scores[i])[::-1]
            top_idx = int(order[0])
            second_idx = int(order[1])
            out = {
                "ID": row.get("ID", ""),
                "UUID": row.get("UUID", ""),
                "Date": row.get("Date", ""),
                "ISO2 Code": row.get("ISO2 Code", ""),
                "Country Title EN": row.get("Country Title EN", ""),
                "Regional Group": row.get("Regional Group", ""),
                "Title EN": clean_value(row.get("Title EN")),
                "top_mechanism_channel": channel_ids[top_idx],
                "top_mechanism_label": channel_labels[top_idx],
                "top_cosine_score": round(float(cosine_scores[i, top_idx]), 6),
                "top_relative_score": round(float(relative_scores[i, top_idx]), 6),
                "second_mechanism_channel": channel_ids[second_idx],
                "second_mechanism_label": channel_labels[second_idx],
                "second_cosine_score": round(float(cosine_scores[i, second_idx]), 6),
                "document_text_chars": len(doc),
            }
            for j, channel in enumerate(channel_ids):
                out[f"{channel}_cosine"] = round(float(cosine_scores[i, j]), 6)
                out[f"{channel}_bounded"] = round(float(bounded_scores[i, j]), 6)
                out[f"{channel}_relative"] = round(float(relative_scores[i, j]), 6)
            writer.writerow(out)

    with LONG_OUT.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "ID",
                "Date",
                "ISO2 Code",
                "Country Title EN",
                "Regional Group",
                "mechanism_channel",
                "mechanism_label",
                "channel",
                "channel_title",
                "cosine_score",
                "bounded_score",
                "relative_score",
            ],
        )
        writer.writeheader()
        for i, row in enumerate(rows):
            for j, channel in enumerate(channel_ids):
                writer.writerow(
                    {
                        "ID": row.get("ID", ""),
                        "Date": row.get("Date", ""),
                        "ISO2 Code": row.get("ISO2 Code", ""),
                        "Country Title EN": row.get("Country Title EN", ""),
                        "Regional Group": row.get("Regional Group", ""),
                        "mechanism_channel": channel,
                        "mechanism_label": label_by_channel[channel],
                        "channel": channel,
                        "channel_title": label_by_channel[channel],
                        "cosine_score": round(float(cosine_scores[i, j]), 6),
                        "bounded_score": round(float(bounded_scores[i, j]), 6),
                        "relative_score": round(float(relative_scores[i, j]), 6),
                    }
                )

    aggregates = defaultdict(
        lambda: {"count": 0, "cosine_sum": 0.0, "bounded_sum": 0.0, "relative_sum": 0.0}
    )
    country_year_counts = defaultdict(int)
    for i, row in enumerate(rows):
        country = clean_value(row.get("Country Title EN")) or (
            "CE / no country title" if clean_value(row.get("ISO2 Code")) == "CE" else ""
        )
        iso2 = clean_value(row.get("ISO2 Code"))
        region = clean_value(row.get("Regional Group"))
        year = clean_value(row.get("Date"))
        cy_key = (country, iso2, region, year)
        country_year_counts[cy_key] += 1
        for j, channel in enumerate(channel_ids):
            key = (*cy_key, channel)
            aggregates[key]["count"] += 1
            aggregates[key]["cosine_sum"] += float(cosine_scores[i, j])
            aggregates[key]["bounded_sum"] += float(bounded_scores[i, j])
            aggregates[key]["relative_sum"] += float(relative_scores[i, j])

    with COUNTRY_YEAR_OUT.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "country",
                "iso2",
                "regional_group",
                "year",
                "mechanism_channel",
                "mechanism_label",
                "channel",
                "channel_title",
                "policy_count",
                "cosine_score_sum",
                "cosine_score_mean",
                "bounded_score_sum",
                "bounded_score_mean",
                "relative_score_sum",
                "relative_score_mean",
            ],
        )
        writer.writeheader()
        for key, values in sorted(aggregates.items()):
            country, iso2, region, year, channel = key
            n = values["count"]
            writer.writerow(
                {
                    "country": country,
                    "iso2": iso2,
                    "regional_group": region,
                    "year": year,
                    "mechanism_channel": channel,
                    "mechanism_label": label_by_channel[channel],
                    "channel": channel,
                    "channel_title": label_by_channel[channel],
                    "policy_count": country_year_counts[(country, iso2, region, year)],
                    "cosine_score_sum": round(values["cosine_sum"], 6),
                    "cosine_score_mean": round(values["cosine_sum"] / n, 6),
                    "bounded_score_sum": round(values["bounded_sum"], 6),
                    "bounded_score_mean": round(values["bounded_sum"] / n, 6),
                    "relative_score_sum": round(values["relative_sum"], 6),
                    "relative_score_mean": round(values["relative_sum"] / n, 6),
                }
            )


def write_report(rows, chunk_count, channels, cosine_scores):
    channel_ids = [channel["channel"] for channel in channels]
    label_by_channel = {channel["channel"]: channel["label"] for channel in channels}
    top_counts = Counter(channel_ids[int(np.argmax(cosine_scores[i]))] for i in range(len(rows)))
    report = [
        "SMECO 2027 hybrid six-channel transformer semantic scores for UNESCO dce002.csv",
        "",
        f"Input records scored: {len(rows)}",
        f"Policy channels scored per policy: {len(channels)}",
        "Text construction: Goal + Main Feature, with short/generic sentences filtered out where possible. Title EN and Area of Monitoring are used only as fallback when substantive content is otherwise empty.",
        "Cultural Domain is excluded so sector/domain labels do not pull mechanism scores.",
        f"Long-text handling: policies were split into chunks of up to {CHUNK_MAX_CHARS} characters before embedding.",
        f"Policy text chunks embedded: {chunk_count}",
        "Scoring method: cosine similarity between each policy text embedding and each final hybrid six-channel definition embedding.",
        "Embedding model: " + EMBEDDING_MODEL_NAME,
        "",
        "Policy channel definitions:",
        *[f"  {channel['channel']}: {channel['label']} - {channel['description']}" for channel in channels],
        "",
        "Top-channel counts, for diagnostic purposes only:",
        *[f"  {channel}: {count} ({label_by_channel[channel]})" for channel, count in top_counts.most_common()],
        "",
        "Output files:",
        f"  {WIDE_OUT}",
        f"  {LONG_OUT}",
        f"  {COUNTRY_YEAR_OUT}",
        f"  {REPORT_OUT}",
    ]
    REPORT_OUT.write_text("\n".join(report), encoding="utf-8")
    print("\n".join(report))


def main():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    channels = load_channels()

    with INPUT.open("r", encoding="utf-8-sig", newline="") as f:
        raw_rows = list(csv.DictReader(f))

    place_pattern = build_place_pattern(raw_rows)
    docs = [build_mechanism_document(row, place_pattern) for row in raw_rows]
    usable = [(row, doc) for row, doc in zip(raw_rows, docs) if doc.strip()]
    rows = [row for row, _ in usable]
    docs = [doc for _, doc in usable]

    os.environ.setdefault("HF_HUB_OFFLINE", "1")
    os.environ.setdefault("TRANSFORMERS_OFFLINE", "1")
    print("Importing sentence_transformers...", flush=True)
    from sentence_transformers import SentenceTransformer

    print("Loading embedding model from local Hugging Face cache...", flush=True)
    model = SentenceTransformer(EMBEDDING_MODEL_NAME)
    channel_texts = [f"{channel['label']}. {channel['description']}" for channel in channels]
    print(f"Scoring {len(docs)} policy documents against {len(channel_texts)} hybrid definitions...", flush=True)
    cosine_scores, chunk_count = score_policy_chunks(model, docs, channel_texts)
    bounded_scores = score_to_0_1(cosine_scores)
    relative_scores = softmax(cosine_scores)

    write_outputs(rows, docs, channels, cosine_scores, bounded_scores, relative_scores)
    write_report(rows, chunk_count, channels, cosine_scores)


if __name__ == "__main__":
    main()
