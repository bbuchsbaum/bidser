# bidser

## Micro-DSL (v2.6) & Output Format

Your output must be **Pure Markdown** that implies the structure defined
by the Micro-DSL. **Do NOT output EBNF token names** (e.g., `H1_TOKEN`).
Generate the actual Markdown (e.g., `# My Title`, `@f my_func(...)`).
**1. Markup Tokens (Implicitly Handled by Markdown):** `H1` = `# text`
`H2` = `## text` `NL` = Newline (use sparingly, primarily to end logical
entries or separate blocks) `HR` = `---` (use to separate major logical
groups within a section, or between sections) `Bul` = `-` (dash + space
for general bullets in Header/Legend/Deps) `IndBul`= `-` (two spaces +
dash + space for indented `Desc` lines under an `@sigil` entry) **2. DSL
Sigils:** `@f` = Function `@d` = Data object (e.g., from
[`data()`](https://rdrr.io/r/utils/data.html)) `@g` = S4/S3 Generic `@m`
= S4/S3 Method (class dispatch) `@c` = R6/R7 constructor (if present)
**3. Other DSL Symbols:** `|` = Separates multiple function names (e.g.,
`name1|name2`) `[...]` = Used for: \* Constructor variants:
`ConstructorName[VariantA|VariantB]` \* Method dispatch classes:
`methodName[ClassA|ClassB]` `(...)` = Parameter list in an entry
signature. `param?` = Optional parameter. `param?=val`= Optional
parameter with a default value. `|` = Separates signature from short
description (after params or name if no params). `->` = Separates short
description from return type. Omit if no return value (side-effect). `!`
= Prefix for inline notes (e.g., `!dep`, `!note: text`). There is no
space between `!` and the note keyword. **4. Document Skeleton:**
`Legend?` (H2 “Legend:” + type abbreviation table) `Header` (H1
PackageName; optional H2 “Core Purpose:”, H2 “Key Objects & Concepts:”)
`Sections+` (H2 `1.` Title; H2 Title (unnumbered ok); optional H3
“Usage:”) `Entries*` (@sigil lines + optional indented bullets) `Deps?`
(H2 “Key R Dependencies:”) **5. Entry Line Structure:**
`@sigil name(|name)*[variant|ClassA|ClassB]? (alias alt1,alt2)? (param1?=val, param2?, ...)? | Short, pithy description -> ReturnTypeAbbr !note_type: Optional note text`
\* **Rules for Entry Line:** \* Omit `()` if no parameters. \* Omit
`-> ReturnTypeAbbr` if function has no return value (side-effect only).
\* Bundle identical signatures using `name1|name2`. \* Use
`ConstructorName[VariantA|VariantB]` for constructor subtypes. \* Use
`methodName[DispatchClassA|DispatchClassB]` for S4/S3 methods. \* Notes
(`!notetype: text` or `!notetype`) are optional postfixes. Ensure no
leading space (e.g., `!ok` not `! ok`). \* Truncate parameter list with
`...` if it exceeds 8 parameters (e.g.,
`(param1, param2, ..., param8, ...)`). \* Example of grouping aliases
and optional params:
`@f read_csv|read_tsv (file, col_types?="auto", ...) | Parse delimited file -> tib`
**6. Indented Description Bullets (`Desc` lines under an Entry):** \*
Format:
`- param_name : type_abbr (constants: val1, "val2" | key_funcs: fnA, fnB)? Brief, essential clarification.`
\* Include `(constants: ...)` for params that take a small, fixed set of
string literals. \* Include `(key_funcs: ...)` for params that expect
specific functions from the package as input. \* **Only include if
adding significant clarity** beyond the signature. Omit for
common/obvious parameters (e.g., `x`, `...`) or standard defaults (e.g.,
`drop=TRUE`). \* If an `Entry` already fits in ≤ 110 characters, do not
add `Desc` lines unless they would prevent ambiguity. \* Can also be
plain text for general notes: `- General descriptive point.` **7. Type
Abbreviations:** \* Use very short (1-4 letter) type abbreviations
(e.g., `NS` for NeuroSpace, `iv` for integer vector, `chr` for
character, `log` for logical, `mat` for matrix, `obj` for generic S4/R6
object). \* Reuse type abbreviations whenever identical across entries;
do not invent synonyms (e.g., use `int` consistently, not `int`, `intv`,
`iv` interchangeably). **7. Type Abbreviations (deprecated – see next
subsection):** \* Use very short (1-4 char) codes, reusing consistently
(e.g., `int` not `intv`). \* Built-in codes: int, dbl, num, chr, lgl,
lst, vec, df, tib, tbl, mat, arr, fn, env, obj, NS \* Provide a
`## Legend:` block only when introducing abbreviations beyond this list.
\## Compression Heuristics & Content Selection: 1. **Focus:** Public,
user-facing API. Omit internal helpers, unexported symbols, and direct
S4 slot accessors (like `slotNames` or methods that just return a slot
value if there’s already a clear getter). Include only symbols present
in NAMESPACE export list. 2. **Grouping:** \* Group trivial getters or
functions with identical signatures and purpose using `name1|name2`. \*
Group constructors with identical fields but different return subtypes
using `ConstructorName[VariantA|VariantB]`. \* Group S3/S4 methods with
identical implementations/docs using `methodName[ClassA|ClassB]`. 3.
**Methods:** Define generics with `@g`. Emit methods (`@m`) **only**
when their behavior, parameters, or return type significantly differ
from the generic, or to explicitly list key supported classes. 4.
**Omissions:** Skip indented parameter descriptions (`Desc` lines) for
obvious defaults (e.g., `drop=TRUE`, `smooth=FALSE`) or very common
arguments like `x` or `...` unless they have package-specific meaning.
The cheatsheet is not full documentation. 5. **Notes:** Use `!` notes
sparingly for critical info (e.g., `!dep`, `!imp`, `!retlist`, `!side`).
6. **Re-exports:** Skip functions and generics re-exported from other
packages (e.g., if
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) is
re-exported, do not list it). \## Output Contract: \* **Pure Markdown
only.** Adhere strictly to the Micro-DSL v2.6. \* No commentary, no
intro/outro paragraphs, no code fences unless part of a
`CODE_BLOCK_TOKEN` within a `BlockContent` (rarely needed for
cheatsheets). \* If any line violates the DSL, regenerate until fully
compliant—no prose explanations. \* Generation stops at first line that
begins with a second `#` at H1 depth (e.g., if `\\n#` is used as a stop
sequence). \* Use a single blank line to separate `Entry` blocks if it
aids readability, but avoid excessive blank lines. Use `---`
(`HR_TOKEN`) to separate major thematic groups within a section or at
the end of sections. \* All parens/brackets/pipes must be balanced. \##
Self-Check (Mental Step - Crucial): Before finalizing, review your
output against these critical checks: 1. Does every content line belong
to a defined DSL structure (Header, Legend, Section, Entry, Desc, Deps,
Block bullet)? 2. Is the DSL syntax for `Entry` lines (sigils, names,
params, `|`, `->`, `!`) correctly used? No bare `->` tokens. 3.
(reserved) 4. For any abbreviation NOT in the built-in list, is it
defined in `## Legend:`? 5. Have you omitted non-essential details and
internal functions?

## Few-shot Exemplar

``` markdown
# dummyPkg
## Legend:
- int : integer
- chr : character
## 1. Core Functions
@f add (x, y) | Sum two ints -> int
```

## Formal Grammar “v2.6 Micro-EBNF”

Cheatsheet ::= Header Legend? Section+ Deps? Legend ::= H2_TOKEN
TEXT_CONTENT NEWLINE_TOKEN Block Header ::= H1_TOKEN TEXT_CONTENT
NEWLINE_TOKEN+ (H2Section) *H2Section ::= H2_TOKEN TEXT_CONTENT
NEWLINE_TOKEN Block Section ::= H2_TOKEN (NUMBER_TOKEN PERIOD_TOKEN)?
TEXT_CONTENT NEWLINE_TOKEN UsageBlock? Entry+ HR_TOKEN? UsageBlock ::=
H3_TOKEN “Usage:” NEWLINE_TOKEN Block Entry ::= Sigil_TOKEN EntryIdent
ParamList? Bar_TOKEN TEXT_CONTENT ArrowReturn Note? NEWLINE_TOKEN Desc*
Sigil_TOKEN ::= AT_F_TOKEN \| AT_D_TOKEN \| AT_G_TOKEN \| AT_M_TOKEN //
Lexer provides @f, @d, @g, @m EntryIdent ::= IdentGroup
MethodOrVariantClass? AliasSpec? IdentGroup ::= IDENT_TOKEN (“\|”
IDENT_TOKEN)\* // For “foo\|bar” MethodOrVariantClass ::= LBRACKET_TOKEN
IDENT_TOKEN (PIPE_TOKEN IDENT_TOKEN)\* RBRACKET_TOKEN // For
“\[ClassA\|ClassB\]” or “\[variantA\|variantB\]” AliasSpec ::=
LPAREN_TOKEN ALIAS_KEYWORD_TOKEN IDENT_TOKEN (COMMA_TOKEN IDENT_TOKEN)\*
RPAREN_TOKEN // For “(alias alt1, alt2)” ParamList ::= LPAREN_TOKEN
Param (COMMA_TOKEN Param)\* RPAREN_TOKEN Param ::= IDENT_TOKEN
(EQUALS_TOKEN DefaultValue)? OPTIONAL_MARKER_TOKEN? DefaultValue ::=
LITERAL_TOKEN \| IDENT_TOKEN ArrowReturn ::= (ARROW_TOKEN IDENT_TOKEN)?
Note ::= EXCLAMATION_TOKEN NOTETYPE_TOKEN (COLON_TOKEN TEXT_CONTENT)?
NEWLINE_TOKEN? Desc ::= INDENT_TOKEN BULLET_MARKER_TOKEN (ParamDesc \|
TEXT_CONTENT) NEWLINE_TOKEN ParamDesc ::= IDENT_TOKEN COLON_TOKEN
TYPE_ABBR_TOKEN ParamExtra? TEXT_CONTENT? ParamExtra ::= LPAREN_TOKEN
(ConstantsSpec \| KeyFuncsSpec) RPAREN_TOKEN ConstantsSpec ::=
“constants:” (IDENT_TOKEN\|LITERAL_TOKEN) (COMMA_TOKEN
(IDENT_TOKEN\|LITERAL_TOKEN)) *KeyFuncsSpec ::= “key_funcs:” IDENT_TOKEN
(COMMA_TOKEN IDENT_TOKEN)* Deps ::= H2_TOKEN “Key R Dependencies:”
NEWLINE_TOKEN Block Block ::= (Bullet \| TEXT_CONTENT \|
CODE_BLOCK_TOKEN)\* (NEWLINE_TOKEN \| EOF_TOKEN) Bullet ::=
BULLET_MARKER_TOKEN TEXT_CONTENT NEWLINE_TOKEN /\* — LEXER-IMPLIED
TOKENS (Illustrative) — All previous tokens from v2.4, plus: AT_G_TOKEN,
AT_M_TOKEN // @g, @m // The lexer provides LBRACKET_TOKEN, IDENT_TOKEN,
PIPE_TOKEN, RBRACKET_TOKEN. // The parser, guided by the Sigil_TOKEN (@f
for constructor variants, @m for method classes), // will interpret the
content of MethodOrVariantClass appropriately. \*/

------------------------------------------------------------------------

## 1. BIDS Project Creation & Structure

### Usage:

- Create a BIDS project from a directory with
  `@f bids_project(path, fmriprep?=FALSE, prep_dir?="derivatives/fmriprep")`.
- For in-memory/mock datasets, use `@f create_mock_bids(...)`.
- Use
  [`participants()`](https://bbuchsbaum.github.io/bidser/reference/participants-method.md),
  [`sessions()`](https://bbuchsbaum.github.io/bidser/reference/sessions-method.md),
  [`tasks()`](https://bbuchsbaum.github.io/bidser/reference/tasks-method.md)
  to query project entities. @f bids_project (path?=“.”,
  fmriprep?=FALSE, prep_dir?=“derivatives/fmriprep”) \| Create BIDS
  project from directory -\> obj
  - fmriprep : lgl (constants: TRUE, FALSE) Include fMRIPrep
    derivatives.
  - prep_dir : chr (constants: “derivatives/fmriprep”) Path to
    derivatives. @f create_mock_bids (project_name, participants,
    file_structure, dataset_description?=NULL, event_data?=lst(),
    confound_data?=lst(), create_stub?=FALSE, stub_path?=NULL,
    prep_dir?=“derivatives/fmriprep”) \| Create in-memory/mock BIDS
    project -\> obj

------------------------------------------------------------------------

## 2. Core Query & File Access

### Usage:

- Use methods like
  [`participants()`](https://bbuchsbaum.github.io/bidser/reference/participants-method.md),
  [`sessions()`](https://bbuchsbaum.github.io/bidser/reference/sessions-method.md),
  [`tasks()`](https://bbuchsbaum.github.io/bidser/reference/tasks-method.md),
  [`flat_list()`](https://bbuchsbaum.github.io/bidser/reference/flat_list-method.md)
  to enumerate project entities.
- Use
  [`func_scans()`](https://bbuchsbaum.github.io/bidser/reference/func_scans.md),
  [`preproc_scans()`](https://bbuchsbaum.github.io/bidser/reference/preproc_scans-method.md),
  [`event_files()`](https://bbuchsbaum.github.io/bidser/reference/event_files-method.md),
  [`confound_files()`](https://bbuchsbaum.github.io/bidser/reference/confound_files-method.md)
  to get file paths by type.
- Use
  [`search_files()`](https://bbuchsbaum.github.io/bidser/reference/search_files.md)
  for flexible file queries by BIDS entities. @m
  participants\[bids_project\|mock_bids_project\] () \| Get participant
  IDs -\> chr @m sessions\[bids_project\|mock_bids_project\] () \| Get
  session IDs -\> chr or NULL @m
  tasks\[bids_project\|mock_bids_project\] () \| Get task names -\> chr
  @m flat_list\[bids_project\|mock_bids_project\] (full_path?=TRUE) \|
  Flatten BIDS tree to file list -\> df
  - full_path : lgl (constants: TRUE, FALSE) Return full file paths. @m
    func_scans\[bids_project\|mock_bids_project\] (subid?=“.*”,
    task?=”.*”, run?=“.*”, session?=”.*”, kind?=“bold”, full_path?=TRUE,
    …) \| Get functional scan file paths -\> chr
  - kind : chr (constants: “bold”) File type. @m
    preproc_scans\[bids_project\|mock_bids_project\] (subid?=“.*”,
    task?=”.*”, run?=“.*”, variant?=NULL, space?=”.*”, session?=“.*”,
    modality?=”bold”, kind?=”.*”, full_path?=FALSE, …) \| Get
    preprocessed scan paths -\> chr @m
    event_files\[bids_project\|mock_bids_project\] (subid?=“.*”,
    task?=”.*”, run?=“.*”, session?=”.*”, full_path?=TRUE, …) \| Get
    event file paths -\> chr @m
    confound_files\[bids_project\|mock_bids_project\] (subid?=“.*”,
    task?=”.*”, session?=“.*”, run?=”.*”, full_path?=FALSE, …) \| Get
    confound file paths -\> chr @f search_files (x, regex?=“.\*“,
    full_path?=FALSE, strict?=TRUE, …) \| Search files by pattern and
    BIDS entities -\> chr
  - strict : lgl (constants: TRUE, FALSE) Require all queried keys to
    exist.

------------------------------------------------------------------------

## 3. Data Reading & Extraction

### Usage:

- Use
  [`read_events()`](https://bbuchsbaum.github.io/bidser/reference/read_events.md),
  [`load_all_events()`](https://bbuchsbaum.github.io/bidser/reference/load_all_events-method.md)
  to read event files as nested or flat tibbles.
- Use
  [`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
  to read confound tables (optionally select variables via `cvars`,
  e.g., `DEFAULT_CVARS`).
- Use `read_func_scans()` and `read_preproc_scans()` to load 4D image
  data (requires `neuroim2`).
- Use
  [`read_sidecar()`](https://bbuchsbaum.github.io/bidser/reference/read_sidecar.md)
  to extract metadata from JSON sidecars. @m
  read_events\[bids_project\|mock_bids_project\] (subid?=“.*”,
  task?=”.*”, run?=“.*”, session?=”.*”, …) \| Read event files as nested
  tibble -\> tib @m read_confounds\[bids_project\|mock_bids_project\]
  (subid?=“.*”, task?=”.*”, session?=“.*”, run?=”.*”,
  cvars?=DEFAULT_CVARS, npcs?=-1, perc_var?=-1, nest?=TRUE, …) \| Read
  confound tables -\> tib
  - cvars : chr (constants: DEFAULT_CVARS) Confound variables to select.
  - nest : lgl (constants: TRUE, FALSE) Return nested tibble. @f
    load_all_events (x, subid?=“.*”, task?=”.*”, run?=“.*”,
    session?=”.*”, full_path?=TRUE, …) \| Read and combine all event
    files -\> tib @f read_func_scans (x, mask, mode?=“normal”,
    subid?=“^sub-.*”, task?=”.*”, run?=“.\*“, modality?=”bold”, …) \|
    Read 4D functional scans -\> obj
  - mode : chr (constants: “normal”, “bigvec”) File reading mode. @f
    read_preproc_scans (x, mask?=NULL, mode?=“normal”, subid?=“^sub-.*”,
    task?=”.*”, run?=“.*”, modality?=”bold”, …) \| Read preprocessed
    scans -\> obj @f read_sidecar (x, subid?=”.*”, task?=“.*”,
    run?=”.*”, session?=“.*”, modality?=”bold”, full_path?=TRUE, …) \|
    Read JSON sidecar metadata -\> tib @f get_repetition_time (x, subid,
    task, run?=”.*”, session?=“.\*“, …) \| Get TR from sidecar JSON -\>
    dbl

------------------------------------------------------------------------

## 4. Masking & Subject Access

### Usage:

- Use
  [`create_preproc_mask()`](https://bbuchsbaum.github.io/bidser/reference/create_preproc_mask.md)
  to create a logical brain mask from preprocessed scans.
- Use
  [`brain_mask()`](https://bbuchsbaum.github.io/bidser/reference/brain_mask.md)
  as a shortcut for mask creation.
- Use
  [`bids_subject()`](https://bbuchsbaum.github.io/bidser/reference/bids_subject.md)
  to get a subject-specific helper interface. @m
  create_preproc_mask\[bids_project\] (subid, thresh?=0.99, …) \| Create
  binary mask from preprocessed scans -\> obj
  - thresh : dbl Threshold for mask inclusion. @m
    brain_mask\[bids_project\] (subid, …) \| Get brain mask for subject
    -\> obj @f bids_subject (x, subid, …) \| Get subject-specific helper
    functions -\> lst

------------------------------------------------------------------------

## 5. Summaries, Checks, and Visualization

### Usage:

- Use
  [`bids_summary()`](https://bbuchsbaum.github.io/bidser/reference/bids_summary.md)
  for dataset statistics.
- Use
  [`bids_check_compliance()`](https://bbuchsbaum.github.io/bidser/reference/bids_check_compliance.md)
  for BIDS compliance checks.
- Use
  [`check_func_scans()`](https://bbuchsbaum.github.io/bidser/reference/check_func_scans.md)
  for scan completeness and file size summaries.
- Use
  [`file_pairs()`](https://bbuchsbaum.github.io/bidser/reference/file_pairs.md)
  to match related files (e.g., BOLD/events).
- Use
  [`plot_bids()`](https://bbuchsbaum.github.io/bidser/reference/plot_bids.md),
  [`bids_heatmap()`](https://bbuchsbaum.github.io/bidser/reference/bids_heatmap.md)
  for visual summaries. @f bids_summary (x) \| Summarize BIDS dataset
  -\> lst @f bids_check_compliance (x) \| Check BIDS compliance -\> lst
  @f check_func_scans (x) \| Inspect scan completeness and sizes -\> lst
  @f file_pairs (x, pair?=“bold-events”, task?=“.\*“, matchon?=c(”run”,
  “task”), …) \| Match related file pairs -\> tib @f plot_bids (x,
  interactive?=TRUE, color_scheme?=“viridis”, include_derivatives?=TRUE,
  file_size_scale?=“log”, highlight_missing?=TRUE,
  visualization_mode?=“standard”, debug?=FALSE) \| Visualize BIDS
  project structure -\> obj @f bids_heatmap (x, interactive?=TRUE,
  color_scheme?=“viridis”, file_type?=“func”, highlight_missing?=TRUE,
  text_size?=2.5, rotate_labels?=TRUE) \| Heatmap visualization of BIDS
  data -\> obj

------------------------------------------------------------------------

## 6. Encoding, Parsing, and Helpers

### Usage:

- Use
  [`encode()`](https://bbuchsbaum.github.io/bidser/reference/encode.md)
  to extract BIDS entities from filenames.
- Use
  [`parse()`](https://bbuchsbaum.github.io/bidser/reference/parse-method.md)
  with a parser object (e.g., from
  [`func_parser()`](https://bbuchsbaum.github.io/bidser/reference/func_parser.md),
  [`anat_parser()`](https://bbuchsbaum.github.io/bidser/reference/anat_parser.md),
  [`bids_parser()`](https://bbuchsbaum.github.io/bidser/reference/bids_parser.md)).
  @f encode (x, …) \| Parse BIDS filename into key-value list -\> lst @f
  parse (x, fname, …) \| Parse filename with parser object -\> lst

------------------------------------------------------------------------

## 7. Exported Data Objects (Constants & Choices)

### Usage:

- Use `DEFAULT_CVARS` for standard confound variable selection in
  [`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md).
- Use `CVARS_ALIASES` for mapping canonical confound names to fMRIPrep
  column names. @d DEFAULT_CVARS \| Standard confound variable names for
  fMRIPrep -\> chr @d CVARS_ALIASES \| Mapping of canonical confound
  variable names to fMRIPrep column aliases -\> lst

------------------------------------------------------------------------

## 8. Plotting (S3 Methods)

@m plot\[bids_project\] (max_depth?=Inf, …) \| Plot BIDS tree as
dendrogram

------------------------------------------------------------------------

## 9. Subject Helper Interface

@m bids_subject\[bids_project\] (subid, …) \| Get subject helper
interface -\> lst

------------------------------------------------------------------------

## 10. Miscellaneous

@f flat_list (x, full_path?=TRUE, …) \| Flatten BIDS tree to file list
-\> df

------------------------------------------------------------------------

# 
