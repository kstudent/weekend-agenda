# weekend-agenda
Provides an org-agenda view that only shows weekends / holidays / workdays.

## Requirements
Org Version > 9.0 (Tested with: 9.0.9, 9.1.6)

## Installation
Place the following line in your .emacs file: 

```
(load "<path-to-weekend-agenda>/weekend-agenda.el")
```

## Usage
In the Agenda List Buffer (named *Org Agenda(a)*, C-c a): 
- Press `v` and then `W` to show work days exclusively. Weekends are defined by `org-agenda-weekend-days`. 
- Press `v` and then `F` to show weekends and holidays exclusively. You can define holidays by creating an item tagged with `:holiday:` and marking it with an active timestamp. 

## Warning
The script overrides the two functions `org-agenda-list` and `org-agenda-view-mode-dispatch` to run the right hooks in the right places. 

