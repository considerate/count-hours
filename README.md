# count-hours

To start tracking time on a project append a line to a csv file with the time
in seconds when you start working together with the project name.

```console
$ echo $(date -Is),count-hours >> ~/.count-hours.csv
```

I personally put this in the `.envrc` (direnv) file for each of the projects
I'm currently working on.

To then count the duration spent on each project run

```console
$ count-hours ~/.count-hours.csv
```

Run `count-hours --help` to obtain a description of the available CLI arguments.

The program assumes an 8 hour working day each weekday when computing the ratio
of working hours. If you need to account for days off use the `--vacations
<days>` flag.
