# FPDocMover

FPDocMover is a easy to use command line tool for merging/joining and splitting [FPDoc](http://www.freepascal.org/docs-html/fpdoc/fpdoc.html) documentation files.

It is wrtitten in [Free Pascal](http://www.freepascal.org/) and runs on all platforms supported by Free Pascal. There are no dependencies on third party libraries.

## Usage Examples
### Joining files
The command is like

    fpdocmover.exe -j -c -f join-list.txt -o data\joined.xml

The file `jon-list.txt` contains a list of all files to merge; each file on a new line
```
; this is a comment
firstfile.xml
# see descritpion of option -c
secondfile.xml

# Empty Lines are ignored also
```

### Splitting files up
This command creates one file for each module/unit in `file.xml`.

    fpdocmover.exe -s -o /home/myname/out -m -i file.xml
    
The output directory `/home/myname/out` has to be an existing direcotry.

If your `file.xml` contains multiple packages, you may want to split up the file by package.

    fpdocmover.exe -s -o /home/myname/out -p -i file.xml

## Parameters
You may specify the parametrs in an arbitrary order.
<pre>
GENERAL
  --help        -h    Shows this help and exits
  --output      -o    Output file name for joins or directory for
                      splitting (default: current directory)

MODES
  --join        -j    Join files
  --split       -s    Split files

JOINING FILES
  --text-nodes  -t    Join text nodes instead of element nodes
  --files       -f    Specify file with list of input file names for joining
  --comment     -c    Allow characters # and ; to start a comment line in file list
  --overwrite   -w    Overwrite existing file

SPLITTING FILES
  --module      -m    Split files by module (default for splitting)
  --package     -p    Split files by package
  --input       -i    Input file name for splitting
</pre>