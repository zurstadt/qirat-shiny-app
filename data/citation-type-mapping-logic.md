# Citation Type Mapping Logic

This document describes the data flow and logic for mapping parsed citations to Zotero/CSL types, based on the z2csl project conventions and the approved schemas in `parser-work-schemas.json`.

**Schema Source**: `/deploy/data/parser-work-schemas.json` (16 approved works)

------------------------------------------------------------------------

## 1. Data Flow Overview

```         
Raw Citation String
       ↓
   [Parser]
       ↓
Parsed Citation Object (author, title_abbrev, volume, page, etc.)
       ↓
   [Type Resolver] ← parser-work-schemas.json
       ↓
Zotero Item Type (book, encyclopediaArticle, webpage, etc.)
       ↓
   [CSL Exporter] ← z2csl typeMap.xml
       ↓
CSL-JSON Output (type: "book", "entry-encyclopedia", "webpage", etc.)
```

------------------------------------------------------------------------

## 2. Zotero → CSL Type Mappings (from z2csl)

The official z2csl project defines these mappings:

| Zotero Type | CSL Type | Used For |
|----|----|----|
| `book` | `book` | Bio-bibliographies, reference works, primary sources |
| `encyclopediaArticle` | `entry-encyclopedia` | Encyclopedia entries (EI1, EI2, EI3, BA, etc.) |
| `webpage` | `webpage` | Database-only resources (PUA) |
| `manuscript` | `manuscript` | Unedited manuscripts |

**Note**: Zotero does not have a native "reference work" or "bio-bibliography" type. These are cited as `book` with specific locator fields.

------------------------------------------------------------------------

## 3. Approved Work Schemas (16 works)

### 3.1 Encyclopedias → `encyclopediaArticle`

| Abbrev | Full Title | Article Authors | Entry Numbers |
|----|----|----|----|
| **BA** | Biblioteca de al-Andalus | `editors_as_authors: true` | Yes (№) |
| **EI1** | E.J. Brill's First Encyclopaedia of Islam | `has_article_authors: true` | No |
| **EI2** | Encyclopaedia of Islam, New Edition | `has_article_authors: true` | No |
| **EI3** | Encyclopaedia of Islam, THREE | `has_article_authors: true` | No |
| **EIr** | Encyclopædia Iranica | `has_article_authors: true` | No |
| **DMBI** | Dāʾirat-i Maʿārif-i Bozorg-i Islāmī | `has_article_authors: true` | No |
| **TDVİA** | Türkiye Diyanet Vakfı İslâm Ansiklopedisi | `has_article_authors: true` | No |

### 3.2 Database → `webpage`

| Abbrev | Full Title | Properties |
|----|----|----|
| **PUA** | Prosopografía de los ulemas de al-Andalus | `is_database: true`, `editors_as_authors: true`, `url: true` |

### 3.3 Bio-bibliographies → `book`

| Abbrev | Full Title | Multi-Edition | Entry Numbers |
|----|----|----|----|
| **GAL** | Geschichte der arabischen Litteratur | German/English (translation) | Yes (alphanumeric) |
| **GAS** | Geschichte des arabischen Schrifttums | No | Yes |
| **GdQ** | Geschichte des Qorans | German/English (translation) | No |

### 3.4 Dual-Edition Works → `book`

| Abbrev | Full Title | Edition Type | Editions |
|----|----|----|----|
| **Fahrasah** | Fahrasat mā rawāhu Ibn Ḫayr al-Išbīlī | critical_edition | Codera / Maʿrūf |
| **ǦB** | Ǧāmiʿ al-bayān fī al-qirāʾāt al-sabʿ | critical_edition | Beirut / Sharjah |

### 3.5 Bibliographic Studies → `book`

| Abbrev   | Full Title                           | Entry Numbers | Footnotes |
|----------|--------------------------------------|---------------|-----------|
| **ĠN**   | Ġāyat al-nihāya fī ṭabaqāt al-qurrāʾ | Yes           | No        |
| **MM**   | Muʿǧam muṣannafāt ʿulūm al-Qurʾān    | No            | Yes       |
| **Adab** | Adab ʿilm al-qirāʾāt                 | Yes           | No        |

------------------------------------------------------------------------

## 4. Field Configuration by Work

### 4.1 Standard Fields

| Field | Description | Used By |
|----|----|----|
| `volume` | Volume number | GAL, GAS, GdQ, ǦB, ĠN, BA, EI1 |
| `page` | Page number(s) | All except EI2, EI3, EIr, DMBI, TDVİA |
| `entry_number` | Entry № | GAL, GAS, ĠN, Adab, BA, PUA, Fahrasah |
| `article_title` | Encyclopedia article title | BA, PUA, TDVİA, EI1-3, EIr, DMBI |
| `article_author` | Article author name | TDVİA, EI1-3, EIr, DMBI |
| `footnote` | Footnote reference | GdQ, MM |
| `url` | Online URL | PUA, TDVİA |

### 4.2 Multi-Edition Pagination

Works with multiple editions use a structured `edition_pagination` configuration:

``` json
"edition_pagination": {
  "pagination_type": "translation",
  "editions": [
    {"key": "german", "label": "2nd German ed. (Brill, 1943-47)", "type": "original"},
    {"key": "english", "label": "Lameer English tr. (Brill, 2016-19)", "type": "translation"}
  ]
}
```

**Approved Multi-Edition Works:**

| Work | Type | Edition 1 | Edition 2 |
|----|----|----|----|
| **GAL** | translation | `german`: "2nd German ed. (Brill, 1943-47)" | `english`: "Lameer English tr. (Brill, 2016-19)" |
| **GdQ** | translation | `german`: "German original" | `english`: "English translation" |
| **Fahrasah** | critical_edition | `codera`: "Cordero-Zaídan/Ribera ed. (Cairo, 1997)" | `maruf`: "Maʿrūf ed. (Tunis, 2009)" |
| **ǦB** | critical_edition | `beirut`: "al-Ǧazāʾirī ed. (Beirut, 2005)" | `sharjah`: "Ṭaḥḥān et al. ed. (Sharjah, 2007)" |

------------------------------------------------------------------------

## 5. Property Flags by Work

| Property | Description | Works |
|----|----|----|
| `has_supplements` | Has supplement volumes | GAL |
| `multi_edition_pagination` | Dual page numbers | GAL, GdQ, Fahrasah, ǦB |
| `alphanumeric_entries` | Entry numbers like №4c | GAL |
| `inherits_author` | Inherits author in `=` citations | GAL, MM |
| `has_article_authors` | Individual entries have authors | EI1, EI2, EI3, EIr, DMBI, TDVİA |
| `editors_as_authors` | Use editors when no article author | BA, PUA |
| `is_reference_work` | Is a reference/finding aid | GAL, GAS, EI1-3, EIr, DMBI, MM |
| `is_database` | Online-only database | PUA, EIr, DMBI |

------------------------------------------------------------------------

## 6. GAL Supplement Handling (Special Case)

### 6.1 Problem Statement

Citation: `Brockelmann, GAL, 1:407/451 №3 = Supplement 1:720/746 №3`

Parser must recognize "Supplement" as an `edition_qualifier`, not a separate `title_abbrev`.

### 6.2 Desired Output

``` json
{
  "references": [
    {
      "title_abbrev": "GAL",
      "edition_qualifier": null,
      "volume": "1",
      "page_german": "407",
      "page_english": "451",
      "entry_number": "3"
    },
    {
      "title_abbrev": "GAL",
      "edition_qualifier": "Supplement",
      "volume": "1",
      "page_german": "720",
      "page_english": "746",
      "entry_number": "3",
      "inherited_title": true
    }
  ]
}
```

### 6.3 Inheritance Rules

For equality citations (`=`), subsequent parts inherit from the first reference:

| Field               | Inheritance Behavior             |
|---------------------|----------------------------------|
| `author`            | Always inherit                   |
| `title_abbrev`      | Inherit unless explicitly stated |
| `zotero_type`       | Always inherit                   |
| `edition_qualifier` | Override (e.g., "Supplement")    |
| `volume`            | Override                         |
| `page_*`            | Override                         |
| `entry_number`      | Override                         |

------------------------------------------------------------------------

## 7. CSL Export Structures

### 7.1 Encyclopedia Article (`entry-encyclopedia`)

``` json
{
  "type": "entry-encyclopedia",
  "title": "Ibn ʿAbd al-Qādir al-Umawī, Abū ʿUmar (№1986)",
  "container-title": "Biblioteca de al-Andalus",
  "author": [
    {"family": "Lirola Delgado", "given": "Jorge"},
    {"family": "Puerta Vílchez", "given": "José Miguel"}
  ],
  "volume": "A",
  "page": "74"
}
```

**Author Handling:** - If `has_article_authors: true` and article_author is provided → use article_author - If `editors_as_authors: true` or article_author is "Editors" → use encyclopedia's editors array

**Entry Number Handling:** - Combine with title: `{article_title} (№{entry_number})` - Example: `"Ibn ʿAbd al-Qādir al-Umawī, Abū ʿUmar (№1986)"`

### 7.2 Webpage/Database Entry (`webpage`)

For PUA and other database-only resources:

``` json
{
  "type": "webpage",
  "title": "Ibn ʿAbd al-Qādir al-Umawī, Abū ʿUmar (№1178)",
  "container-title": "Prosopografía de los ulemas de al-Andalus",
  "author": [
    {"family": "Ávila", "given": "María Luisa"},
    {"family": "Molina", "given": "Luis"},
    {"family": "Penelas", "given": "Mayte"},
    {"family": "López Fernández", "given": "María"}
  ],
  "URL": "https://archivo.eea.csic.es/pua/personaje/consulta_personaje.php?id=1178",
  "accessed": {"date-parts": [[2026, 1, 27]]}
}
```

### 7.3 Book with Multi-Edition Pages (`book`)

``` json
{
  "type": "book",
  "title": "Geschichte der arabischen Litteratur",
  "author": [{"family": "Brockelmann", "given": "Carl"}],
  "volume": "1"
}
```

**Multi-Edition Pages in Zotero Extra Field:**

```         
Page (2nd German ed.): 407
Page (Lameer English tr.): 451
Entry: 3
```

------------------------------------------------------------------------

## 8. Work-Specific Notes (from Approved Schemas)

| Work | Notes |
|----|----|
| **BA** | Every entry should have an author, whether it is expressly named or uses the "Editors" placeholder. |
| **TDVİA** | I have not provided any URLs in my examples, but all of these are available online and I would eventually like to add these. |
| **EI2, EI3** | I have not provided any DOIs in my examples, but all of these are available online and I would eventually like to add these. |
| **EIr, DMBI** | I have not provided any URLs in my examples, but all of these are available online and I would eventually like to add these. |
| **Fahrasah** | Ensure that the entry_number is associated only with the Maʿrūf edition. |

------------------------------------------------------------------------

## 9. Non-Standard Fields → Zotero Extra

Fields without standard Zotero equivalents are stored in the Extra field:

| Parser Field        | Extra Field Format                |
|---------------------|-----------------------------------|
| `entry_number`      | `Entry: {value}`                  |
| `section`           | `Section: {value}`                |
| `edition_qualifier` | `Edition-Qualifier: {value}`      |
| `footnote`          | `Note: fn. {value}`               |
| `page_{edition}`    | `Page ({edition_label}): {value}` |

------------------------------------------------------------------------

## 10. Reference: z2csl Project

-   **Repository**: <https://github.com/aurimasv/z2csl>
-   **Live Type Map**: <https://aurimasv.github.io/z2csl/typeMap.xml>
-   **Purpose**: Official Zotero-to-CSL field and type mapping definitions
-   **Version Used**: 6.0.10-beta.7 (2022-07-14)

The z2csl mappings are authoritative for Zotero↔CSL conversion. Custom fields (dual pagination, entry numbers) require project-specific handling outside the standard mapping.

------------------------------------------------------------------------

## Appendix: Complete Approved Schema Summary

| Abbrev | Zotero Type | CSL Type | Multi-Edition | Entry № | Approved |
|----|----|----|----|----|----|
| BA | encyclopediaArticle | entry-encyclopedia | — | Yes | 2026-01-27 |
| PUA | webpage | webpage | — | Yes | 2026-01-27 |
| TDVİA | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
| GAL | book | book | translation | Yes (alphanumeric) | 2026-01-27 |
| GAS | book | book | — | Yes | 2026-01-27 |
| GdQ | book | book | translation | — | 2026-01-27 |
| Fahrasah | book | book | critical_edition | Yes | 2026-01-27 |
| ǦB | book | book | critical_edition | — | 2026-01-27 |
| ĠN | book | book | — | Yes | 2026-01-27 |
| MM | book | book | — | — | 2026-01-27 |
| Adab | book | book | — | Yes | 2026-01-27 |
| EI1 | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
| EI2 | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
| EI3 | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
| EIr | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
| DMBI | encyclopediaArticle | entry-encyclopedia | — | — | 2026-01-27 |
