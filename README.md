# covid_report
Covid COBOL report

== COVID19 Report ==
The COVID19 program will read a csv file, separate the fields and format a reporton SYSOUT.  The input file has been defined as having variable length records.


== ZOWE commands ==

=== file commands ===

Download source/jcl from PDS to a folder:
zowe files download am " Z81535.CBL" -e ".cbl"
zowe files download am " Z81535.JCL" -e ".jcl"

Replace source file on PDS with one on laptop:
zowe zos-files upload file-to-data-set covid19.cobol Z81535.CBL

Create dataset for variable length records:
zowe zos-files create ps Z81535.COVID19 --rf VB --rl 400

Upload file to dataset:
zowe zos-files ul ftds "covid19a.csv" "z81535.covid19"  

Download dataset:
zowe zos-files download ds z81535.covid19 -f test.csv


=== job commands ===
To submit the job, wait for it to complete,
and view all spool content, issue:
zowe jobs submit ds " Z81535.JCL(HELLO)" --vasc

To submit the job and wait for it to enter OUTPUT status,
issue:
zowe jobs submit ds "Z81535.JCL(HELLO)" --wfo

To list spool files associated with this job id, issue:
zowe jobs list sfbj JOB00906
where JOB00906 was returned from the previous command.

To view a specific spool file (COBRUN:SYSOUT), issue:
zowe jobs view sfbi JOB00906 105
where JOB00906 and 105 are obtained from the previous commands.

If desired, you can also easily submit a job, wait for it to complete, and download the spool content using the
following command (see the following figure for the completed state):
zowe jobs submit ds " Z81535.JCL(HELLO)" -d .

Use the -rfj flag to receive the output in JSON format:
zowe jobs submit data-set "Z81535.JCL(HELLO)" -d . --rfj

Use the package.jason to submit a job, change the 'script name' and value:
npm run 'script name'
