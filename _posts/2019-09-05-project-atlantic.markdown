---
layout: post
title: PROJECT_ATLANTIC
date: '2018-08-29 10:26'
---

## Project Atlantic

### Ideas & Todos

1. **General**

- [x] Autocomplete
- [x] Case insensitive search
- [ ] Drag and Drop for files
- [x] Sort tables
- [x] Sort int, float, string, date string (ISO8601 and European)
- [x] Sort Currencies (Supported: Dollar (`$`) and Euro (`€`) in formats: "3,00 $", "3.00 $", "3,00$", "3.00$")
- [x] Support ASC and DESC sorting a table column
- [ ] Support parsing of different float separators (like 3.234,00 or 3,450.23)
- [x] Add decoder for previous `TableDataTagged` (without field `dataFormats`) if some user have the old encoding
- [ ] Support for multiple files
  - ~~Either have tables for each file~~
  - Or introduce sessions per table type and enforce table headings and data formats to be equal in each session

2. **File import**
- [x] Add possibility to stack all imported data or to replace remaining (per file import)
- [x] Create file import process documentation
- [ ] Implement process
- [ ] Workspace feature



### Backlog

- [ ] Add tag on enter
- [ ] Add redo functionality with list of Msg's
- [ ] Migrate from localstorage to indexedDB