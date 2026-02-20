#!/usr/bin/env node
/**
 * Distill EcoAtlas snippets into token-light "microcards" using OpenAI Responses API
 * with Structured Outputs (json_schema + strict: true).
 *
 * Env:
 *   OPENAI_API_KEY              (required)
 *   ECO_ATLAS_MODEL             (default: gpt-4o-mini)
 *   ECO_ATLAS_OUT               (default: atlas)
 *   ECO_ATLAS_REQUESTS_PER_RUN  (default: 60)  — cost control
 *   ECO_ATLAS_MAX_CARDS_PER_SNIPPET (default: 2)
 */

import fs from "node:fs";
import path from "node:path";

const OUT_DIR  = process.env.ECO_ATLAS_OUT || "atlas";
const MODEL    = process.env.ECO_ATLAS_MODEL || "gpt-4o-mini";
const MAX_REQ  = parseInt(process.env.ECO_ATLAS_REQUESTS_PER_RUN || "60", 10);
const MAX_CARDS = parseInt(process.env.ECO_ATLAS_MAX_CARDS_PER_SNIPPET || "2", 10);
const INCLUDE_TEST_CARDS = ["1", "true", "yes"].includes(
  String(process.env.ECO_ATLAS_INCLUDE_TEST_CARDS || "0").toLowerCase()
);

const OPENAI_API_KEY = process.env.OPENAI_API_KEY;
if (!OPENAI_API_KEY) { console.error("Missing OPENAI_API_KEY."); process.exit(1); }

const SNIPPETS_PATH = path.join(OUT_DIR, "snippets.jsonl");
const SYMBOLS_PATH  = path.join(OUT_DIR, "symbols.jsonl");
const CARDS_PATH    = path.join(OUT_DIR, "cards.jsonl");
const CACHE_PATH    = path.join(OUT_DIR, ".cards_cache.json");

function readJsonl(filePath) {
  if (!fs.existsSync(filePath)) return [];
  return fs.readFileSync(filePath, "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l));
}

function writeJsonl(filePath, records) {
  fs.writeFileSync(filePath, records.map((r) => JSON.stringify(r)).join("\n") + "\n", "utf8");
}

function loadCache() {
  if (!fs.existsSync(CACHE_PATH)) return {};
  try { return JSON.parse(fs.readFileSync(CACHE_PATH, "utf8")); } catch { return {}; }
}

function saveCache(cache) {
  fs.writeFileSync(CACHE_PATH, JSON.stringify(cache, null, 2), "utf8");
}

function extractOutputText(resp) {
  for (const item of resp.output || []) {
    if (item.type === "message" && item.role === "assistant") {
      for (const c of item.content || []) {
        if (c.type === "output_text" && typeof c.text === "string") return c.text;
      }
    }
  }
  return null;
}

function clampWords(text, maxWords) {
  const words = (text || "").trim().split(/\s+/);
  return words.length <= maxWords ? text : words.slice(0, maxWords).join(" ") + "…";
}

function clampLines(text, maxLines) {
  const lines = (text || "").split("\n");
  return lines.length <= maxLines ? text : lines.slice(0, maxLines).join("\n") + "\n# ...";
}

function normalizeQuestion(text) {
  const trimmed = (text || "").trim();
  if (!trimmed) return "";
  const q = trimmed.endsWith("?") ? trimmed : `${trimmed}?`;
  if (/^how do i\b/i.test(q)) return q;
  return `How do I ${q.charAt(0).toLowerCase()}${q.slice(1)}`;
}

function hasSymbolMention(recipe, symbols) {
  const code = recipe || "";
  for (const sym of symbols || []) {
    const fn = sym.includes("::") ? sym.split("::")[1] : sym;
    if (code.includes(sym) || code.includes(`${fn}(`)) return true;
  }
  return false;
}

function looksLikeTestRecipe(recipe) {
  const code = recipe || "";
  return /(test_that\s*\(|expect_[a-z_]+\s*\(|stopifnot\s*\(|snapshot_)/i.test(code);
}

const symbols  = readJsonl(SYMBOLS_PATH);
const snippets = readJsonl(SNIPPETS_PATH);

const manifest = fs.existsSync(path.join(OUT_DIR, "manifest.json"))
  ? JSON.parse(fs.readFileSync(path.join(OUT_DIR, "manifest.json"), "utf8"))
  : { package: "unknownpkg", language: "R" };

const exportedSymbols = symbols.map((s) => s.symbol);
const cache = loadCache();

// Evict stale cache entries
const snippetHashes = new Set(snippets.map((s) => s.hash));
for (const h of Object.keys(cache)) {
  if (!snippetHashes.has(h)) delete cache[h];
}

// Rank: eco > vignette > readme > testthat
const kindPriority = { eco: 1, vignette: 2, readme: 3, testthat: 4 };
snippets.sort((a, b) => (kindPriority[a.kind] || 9) - (kindPriority[b.kind] || 9));

// JSON schema for Structured Outputs
const schema = {
  type: "object",
  additionalProperties: false,
  properties: {
    cards: {
      type: "array",
      maxItems: MAX_CARDS,
      items: {
        type: "object",
        additionalProperties: false,
        properties: {
          q:       { type: "string", minLength: 8 },
          a:       { type: "string", minLength: 20 },
          recipe:  { type: "string", minLength: 10 },
          tags:    { type: "array", items: { type: "string" } },
          symbols: { type: "array", items: { type: "string" }, minItems: 1 }
        },
        required: ["q", "a", "recipe", "tags", "symbols"]
      }
    }
  },
  required: ["cards"]
};

async function callOpenAI({ instructions, input }) {
  const res = await fetch("https://api.openai.com/v1/responses", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${OPENAI_API_KEY}`
    },
    body: JSON.stringify({
      model: MODEL,
      instructions,
      input,
      temperature: 0.2,
      text: { format: { type: "json_schema", name: "eco_microcards", strict: true, schema } }
    })
  });

  if (!res.ok) { const t = await res.text(); throw new Error(`OpenAI error ${res.status}: ${t}`); }
  return res.json();
}

function normalizeAllowedSymbols(snippet) {
  const allowed = new Set(exportedSymbols);
  for (const s of snippet.symbols || []) {
    allowed.add(s.includes("::") ? s : `${manifest.package}::${s}`);
  }
  return Array.from(allowed).slice(0, 200);
}

let requests = 0;

for (const snip of snippets) {
  if (!INCLUDE_TEST_CARDS && snip.kind === "testthat") {
    continue;
  }
  if (cache[snip.hash]) continue;
  if (requests >= MAX_REQ) break;

  const allowedSymbols = normalizeAllowedSymbols(snip);

  const instructions =
`You are generating token-light "How do I...?" microcards for an internal package ecosystem.
Rules:
- Output MUST match the provided JSON schema (strict).
- Do NOT invent functions. Only mention symbols from the allowed list.
- Prefer canonical package workflows over toy examples.
- Keep each answer <= 100 words. Keep recipe <= 15 lines.
- Question must be specific and actionable; avoid generic wording.
- Answer should mention the primary function and expected output.
- Recipe must include at least one concrete function call from the selected symbols.
- Use explicit package namespace where possible (pkg::fn).
- Avoid test-only instructions (expect_*, test_that, snapshots) unless unavoidable.
- Prefer a single best card. Return 0 cards if the snippet is not actionable.
- Focus on canonical workflow and intent; minimize detail.`;

  const input =
`PACKAGE: ${snip.package}
LANGUAGE: ${snip.language}
KIND: ${snip.kind}
TITLE: ${snip.title}
QUESTION_SEED: ${snip.question_seed || ""}

ALLOWED_SYMBOLS:
${allowedSymbols.join("\n")}

SNIPPET_CODE:
${snip.code}`;

  try {
    const resp   = await callOpenAI({ instructions, input });
    const text   = extractOutputText(resp);
    if (!text) throw new Error("No output_text in response.");

    const parsed = JSON.parse(text);
    const cards  = Array.isArray(parsed.cards) ? parsed.cards : [];
    const allowedSet = new Set(allowedSymbols);

    const cleaned = cards.map((c, idx) => {
      const syms = (c.symbols || []).filter((s) => allowedSet.has(s));
      if (syms.length === 0) return null;
      const q = normalizeQuestion((c.q || "").trim());
      const a = clampWords((c.a || "").trim(), 100);
      const recipe = clampLines((c.recipe || "").trim(), 15);
      if (!q || !a || !recipe) return null;
      if ((a.split(/\s+/).filter(Boolean).length || 0) < 10) return null;
      if (looksLikeTestRecipe(recipe)) return null;
      if (!hasSymbolMention(recipe, syms)) return null;

      const tags = [...new Set((c.tags || []).map((t) => String(t).trim().toLowerCase()).filter(Boolean))];
      return {
        id:           `${snip.id}#${idx + 1}`,
        package:      snip.package,
        language:     snip.language,
        q:            clampWords(q, 22),
        a,
        recipe,
        tags,
        symbols:      syms,
        kind:         snip.kind,
        snippet_id:   snip.id,
        snippet_hash: snip.hash,
        sources:      [snip.source]
      };
    }).filter(Boolean);

    cache[snip.hash] = cleaned;
    requests += 1;
    console.error(`Distilled ${cleaned.length} card(s) from ${snip.id}`);
  } catch (e) {
    console.error(`Failed to distill ${snip.id}: ${e.message}`);
    cache[snip.hash] = [];
    requests += 1;
  }
}

saveCache(cache);

const allCards = [];
for (const snip of snippets) {
  for (const c of cache[snip.hash] || []) allCards.push(c);
}
writeJsonl(CARDS_PATH, allCards);
console.error(`Wrote ${allCards.length} total microcards to ${CARDS_PATH}`);
