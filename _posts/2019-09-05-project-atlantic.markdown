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
  - Or introduce workspace per table type and enforce table headings ~~and data formats~~ to be equal in each workspace

2. **File import process**
- [x] Add possibility to stack all imported data or to replace remaining (per file import)
- [x] Create file import process documentation
- [x] Implement process
- [ ] Update user documentation

3. **View all untagged workspace data**
- [ ] Show nav icon in tagging section
- [ ] Show dialog with all untagged data after click on nav icon

5. **Automatic assignment**

- [ ] Add possibility to create an automatic assignment rule when in batch tagging dialog
- [ ] Show all rules in the batch tagging tab with option to en-/disable them
- [ ] Add possibility to reorder the rules
- [ ] Automatically apply (in given order) active rules to new and successful file imports
- [ ] Notify the user after a rule has applied and done it's job
- [ ] Update user documentation

6. **Suggestion assignment**

7. **Workspaces**

- [ ] Migrate from localstorage to indexedDB

  - [ ] Migration routine for previous user data

  - https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API
  - https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Basic_Concepts_Behind_IndexedDB
  - https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB

- [ ] Separate workspaces by creating an object store for each workspace ?

### Backlog

- [ ] Add tag on enter
- [ ] Add redo functionality with list of Msg's
- [x] Convienence buttons for dataformats (place regex)
- [ ] Multiple tags for each data set