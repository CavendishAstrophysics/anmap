C
C
*+ clean_batch_report

       subroutine clean_batch_report( string )
C      ---------------------------------------
C
C Report progress of clean to user
C
C Given:
C   character string to send
       character*(*)    string
C
C The character string STRING is sent as a message to the user who owns
C the current CLEAN job.
C-
       include '/mrao/include/chrlib_functions.inc'
C
       integer         status
       character*80    comment
       character*80    job_name, job_task, user_name
       common /clean_batch_rep_incl/ job_name, job_task, user_name,
     *                               comment

       status = 0
       call io_wrout(string(1:chr_lenb(string)))

       end
