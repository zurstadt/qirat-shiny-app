# Brockelmann GAL Parser Analysis

## 1. Citation Pattern Analysis

From `gal-examples.txt`, the Brockelmann GAL citations follow these patterns:

### 1.1 Basic GAL Citation (Main Edition)
```
Brockelmann, GAL, 1:407/451 №3
```
Structure: `Author, Title, Volume:PageGerman/PageEnglish №EntryNumber`

### 1.2 GAL Supplement Citation
```
Brockelmann, GAL, Supplement 1:721/747 №4c
```
Structure: `Author, Title, [Supplement] Volume:PageGerman/PageEnglish №EntryNumber`

### 1.3 Equality Citation (Main = Supplement)
```
Brockelmann, GAL, 1:407/451 №3 = Supplement 1:720/746 №3
```
Structure: Two references linked by `=`, second inherits author and base title

### 1.4 Variant: Missing Volume in Supplement
```
Brockelmann, GAL, Supplement 724/750 №11d  (line 12)
Brockelmann, GAL, 729/755–56 №2a  (line 15)
```
Volume may be omitted when implicitly understood from context.

### 1.5 Variant: Entry Number Placement
```
Brockelmann, GAL, 1:412 №7/458 №6  (line 7)
```
Entry numbers can appear after both German and English page numbers.

### 1.6 Edition Qualifier (Ger./Eng.)
```
Brockelmann, GAL (Ger.), Supplement 1:722 №6c = GAL (Eng.), 1:452 №6  (line 21)
```
Some citations distinguish German vs English editions explicitly.

---

## 2. Current Parser Issues

### 2.1 Supplement Treated as Separate Title
**Problem**: In equality citations, "Supplement" is parsed as `title_abbrev` instead of recognizing it as a qualifier for GAL.

From `adjudicated_citations_20260126.json`:
```json
{
  "original": "Brockelmann, GAL, 1:407/451 №3 = Supplement 1:720/746 №3",
  "references": [
    {
      "author": "Brockelmann",
      "title_abbrev": "GAL",           // CORRECT
      "volume": "1",
      "page": "407"
    },
    {
      "author": "Brockelmann",
      "title_abbrev": "Supplement",    // WRONG - should be "GAL, Supplement"
      "volume": "1",
      "page": "720"
    }
  ]
}
```

**Root Cause**: `parse_short_equality()` splits on ` = ` and parses subsequent parts as standalone references, extracting the first word as `title_abbrev`.

### 2.2 No Supplement Field in Schema
The current schema lacks a dedicated field to indicate whether a citation refers to the main GAL volumes or the Supplement volumes.

### 2.3 Work Type Not Categorized
The parser has `is_primary: TRUE/FALSE` but lacks fine-grained categorization:
- **Reference works**: GAL, GAS, GdQ, EI2, TDVİA, etc.
- **Research monographs**: Modern scholarly studies
- **Primary sources**: Original texts being cited

---

## 3. Proposed Schema Changes

### 3.1 Add `edition_qualifier` Field
```r
edition_qualifier = NA,  # "Supplement", "Ger.", "Eng.", etc.
```

For GAL citations, this would capture:
- `"Supplement 1"` → `edition_qualifier = "Supplement"`, `volume = "1"`
- `"GAL (Ger.)"` → `edition_qualifier = "Ger."`

### 3.2 Add `work_category` Field
```r
work_category = NA,  # "reference", "monograph_study", "primary_source"
```

Classification:
| Abbreviation | Category |
|-------------|----------|
| GAL, GAS, GdQ | reference |
| EI2, EI3, TDVİA, EIr | reference (encyclopedia) |
| MM, Adab, MQM, MQK | reference (bibliographic study) |
| Fihrist, Fahrasah | reference (finding aid) |
| Modern monographs | monograph_study |
| Primary texts | primary_source |

### 3.3 Maintain Inheritance Chain
For equality citations, subsequent parts should inherit:
- `author` (always)
- `title_abbrev` (if not explicitly stated)
- `work_category` (always)

New explicit values override inherited ones:
- `edition_qualifier` (e.g., "Supplement")
- `volume`
- `page_german`, `page_english`
- `entry_number`

---

## 4. Recommended Parser Changes

### 4.1 Detect GAL Supplement Pattern
Add detection in `parse_short_equality()` and `parse_short_citation()`:

```r
# Pattern: "Supplement [Volume:]Page/Page №Entry"
gal_supplement_pattern <- "^Supplement\\s+(\\d+:)?(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?"

# When detected, set:
# - edition_qualifier = "Supplement"
# - inherit title_abbrev from previous reference (GAL)
```

### 4.2 Detect Edition Qualifier (Ger./Eng.)
```r
# Pattern: "GAL (Ger.)" or "GAL (Eng.)"
edition_qual_pattern <- "\\(([A-Za-z]+\\.?)\\)"
```

### 4.3 Handle Inheritance in Equality Citations
Modify `parse_short_equality()` to track inherited fields:

```r
# For subsequent parts (i > 1):
if (starts_with_supplement(part)) {
  ref$title_abbrev <- previous_ref$title_abbrev  # Inherit "GAL"
  ref$edition_qualifier <- "Supplement"
  ref$inherited_title <- TRUE
}
```

### 4.4 Add Work Category Assignment
```r
assign_work_category <- function(title_abbrev, author) {
  reference_works <- c("GAL", "GAS", "GdQ", "EI2", "EI3", "TDVİA", "EIr",
                       "DMBI", "BA", "MM", "Adab", "MQM", "MQK", "ĠN",
                       "Fihrist", "Fahrasah")

  if (title_abbrev %in% reference_works) {
    return("reference")
  }

  # Check author for known reference work compilers
  reference_authors <- c("Brockelmann", "Sezgin", "Nöldeke", "Bergsträsser",
                         "Pretzl", "Ḥamdān", "Sālim")
  if (author %in% reference_authors) {
    return("reference")
  }

  return("monograph_study")  # Default for modern scholarly works
}
```

---

## 5. Expected Output Schema

For: `Brockelmann, GAL, 1:407/451 №3 = Supplement 1:720/746 №3`

```json
{
  "type": "short_equality",
  "author": "Brockelmann",
  "work_category": "reference",
  "n_references": 2,
  "references": [
    {
      "reference_number": 1,
      "author": "Brockelmann",
      "title_abbrev": "GAL",
      "edition_qualifier": null,
      "volume": "1",
      "page_german": "407",
      "page_english": "451",
      "entry_number": "3",
      "work_category": "reference",
      "inherited_author": false,
      "inherited_title": false
    },
    {
      "reference_number": 2,
      "author": "Brockelmann",
      "title_abbrev": "GAL",
      "edition_qualifier": "Supplement",
      "volume": "1",
      "page_german": "720",
      "page_english": "746",
      "entry_number": "3",
      "work_category": "reference",
      "inherited_author": true,
      "inherited_title": true
    }
  ]
}
```

---

## 6. Test Cases

| Input | Expected title_abbrev | Expected edition_qualifier |
|-------|----------------------|---------------------------|
| `Brockelmann, GAL, 1:407/451 №3` | GAL | null |
| `Brockelmann, GAL, Supplement 1:721/747 №4c` | GAL | Supplement |
| `Supplement 1:720/746 №3` (after `=`) | GAL (inherited) | Supplement |
| `GAL (Ger.), Supplement 1:722 №6c` | GAL | Supplement, Ger. |
| `GAL (Eng.), 1:452 №6` | GAL | Eng. |

---

## 7. Priority of Changes

1. **HIGH**: Fix Supplement inheritance in equality citations
2. **HIGH**: Add `edition_qualifier` field to schema
3. **MEDIUM**: Add `work_category` classification
4. **MEDIUM**: Handle edition qualifiers (Ger./Eng.)
5. **LOW**: Handle missing volume numbers (default to context or 1)

---

## 8. Files to Modify

1. `shiny-app/deploy/citation_parsers.R`:
   - `parse_short_citation()` - Add GAL Supplement detection
   - `parse_short_equality()` - Fix inheritance for Supplement
   - Add `assign_work_category()` function
   - Update result schema with new fields

2. `deploy/training-bibliography/` - Add test cases for GAL patterns

3. `shiny-app/deploy/test_parsing.R` - Add unit tests for GAL citations
