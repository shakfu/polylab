# makefilegen

A Makefile generator / direct compilation tool, extracted from my work in shedskin.makefile in the [shedskin project](https://github.com/shedskin/shedskin), as this generic part of the code may be generally useful.


## TODO

- [ ] Maybe use `unique_list` datastructure

- [ ] Include a referance to auto variables

```python
AUTOMATIC_VARIABLES = {
    "$@": "The file name of the target of the rule.",
    "$%": "The target member name, when the target is an archive member.",
    "$<": "The name of the first prerequisite.",
    "$?": "The names of all the prerequisites that are newer than the target, with spaces between them.",
    "$^": "The names of all the prerequisites, with spaces between them.",
    "$+": "This is like `$^`, but prerequisites listed more than once are duplicated in the order they were listed in the makefile.",
    "$|": "The names of all the order-only prerequisites, with spaces between them.",
    "$*": "The stem with which an implicit rule matches.",
    "$(@D)": "The directory part of the file name of the target, with the trailing slash removed.",
    "$(@F)": "The file-within-directory part of the file name of the target.",
    "$(*D)": "The directory part of the stem.",
    "$(*F)": "The file-within-directory part of the stem.",
    "$(%D)": "The directory part of the target archive member name.",
    "$(%F)": "The file-within-directory part of the target archive member name.",
    "$(^D)": "Lists of the directory parts of all prerequisites.",
    "$(^F)": "Lists of the file-within-directory parts of all prerequisites.",
    "$(+D)": "Lists of the directory parts of all prerequisites, including multiple instances of duplicated prerequisites.",
    "$(+F)": "Lists of the file-within-directory parts of all prerequisites, including multiple instances of duplicated prerequisites.",
    "$(?D)": "Lists of the directory parts of all prerequisites that are newer than the target.",
    "$(?F)": "Lists of the file-within-directory parts of all prerequisites that are newer than the target.",
}
```
