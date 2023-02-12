PARAMETER m.skeleton, m.prompt,  ;
          m.newfile, m.allfiles,  ;
          m.chdir, m.chdrive,  ;
          m.numdrive
PRIVATE ALL EXCEPT m.skeleton
PRIVATE m.currarea, m.talkstat,  ;
        m.compstat
IF SET('TALK') = 'ON'
     SET TALK OFF
     m.talkstat = 'ON'
ELSE
     m.talkstat = 'OFF'
ENDIF
m.compstat = SET('COMPATIBLE')
SET COMPATIBLE OFF
m.currarea = SELECT()
IF  .NOT. WEXIST('_qh30vsr09')
     DEFINE WINDOW _qh30vsr09  ;
            FROM INT((SROWS() -  ;
            17) / 2),  ;
            INT((SCOLS() - 48) /  ;
            2) TO INT((SROWS() -  ;
            17) / 2) + 16,  ;
            INT((SCOLS() - 48) /  ;
            2) + 47 FLOAT NOCLOSE  ;
            SHADOW DOUBLE COLOR  ;
            SCHEME 5
ENDIF
FOR m.i = PARAMETERS() + 1 TO 7
     DO CASE
          CASE m.i = 1
               m.skeleton = '*.*'
          CASE m.i = 2
               m.prompt = 'Seleccione Archivo:'
          CASE m.i = 3
               m.newfile = .F.
          CASE m.i = 4
               m.allfiles = .T.
          CASE m.i = 5
               m.chdir = .T.
          CASE m.i = 6
               m.chdrive = .T.
          CASE m.i = 7
               m.numdrive = 26
     ENDCASE
ENDFOR
FOR m.i = 1 TO PARAMETERS()
     DO CASE
          CASE m.i = 1
               m.skeleton = IIF(  ;
                            .NOT.  ;
                            TYPE('m.skeleton') =  ;
                            'C',  ;
                            '*.*',  ;
                            m.skeleton)
          CASE m.i = 2
               m.prompt = IIF(  ;
                          .NOT.  ;
                          TYPE('m.prompt') =  ;
                          'C',  ;
                          'Seleccione Archivo:',  ;
                          m.prompt)
          CASE m.i = 3
               m.newfile = IIF(  ;
                           .NOT.  ;
                           TYPE('m.newfile') =  ;
                           'L',  ;
                           .F.,  ;
                           m.newfile)
          CASE m.i = 4
               m.allfiles = IIF(  ;
                            .NOT.  ;
                            TYPE('m.allfiles') =  ;
                            'L',  ;
                            .T.,  ;
                            m.allfiles)
          CASE m.i = 5
               m.chdir = IIF(  ;
                         .NOT.  ;
                         TYPE('m.chdir') =  ;
                         'L', .T.,  ;
                         m.chdir)
          CASE m.i = 6
               m.chdrive = IIF(  ;
                           .NOT.  ;
                           TYPE('m.chdrive') =  ;
                           'L',  ;
                           .T.,  ;
                           m.chdrive)
          CASE m.i = 7
               m.numdrive = IIF(  ;
                            .NOT.  ;
                            TYPE('m.NumDrive') =  ;
                            'N',  ;
                            26,  ;
                            m.numdrive)
     ENDCASE
ENDFOR
m.savepath = SET('default') +  ;
             CURDIR()
m.isdiskin = .F.
IF  .NOT. EMPTY(m.isdiskin)
ENDIF
DIMENSION a_files[ 1, 1]
DIMENSION a_drives[ 1, 1]
DIMENSION a_dirs[ 1, 1]
m.l_files = 0
m.o_drives = ''
m.o_dirs = 0
m.k_allfiles = .F.
DO iarray WITH 'files',  ;
   m.skeleton, m.chdir
m.l_files = IIF( .NOT.  ;
            EMPTY(a_files(1,1)),  ;
            1, 0)
DO iarray WITH 'drives'
m.o_drives = a_drives(ASCAN(a_drives,  ;
             SYS(5)),1)
DO iarray WITH 'path'
m.o_dirs = ALEN(a_dirs, 1)
IF WVISIBLE('_qh30vsr09')
     ACTIVATE WINDOW SAME  ;
              _qh30vsr09
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              _qh30vsr09
ENDIF
@ 0, 3 SAY m.prompt SIZE 1, 40
@ 1,2 get m.l_files  picture "@&N";
 from a_files  size 13,16  default 1;
 when _qh30vstwl()  valid _qh30vsu0l();
 color scheme 6
@ 14, 3 GET m.k_allfiles DEFAULT  ;
  0 SIZE 1, 22 PICTURE  ;
  '@*C Todos los Archivos' VALID  ;
  _qh30vsu9k()
@ 3, 26 SAY 'Drive'
@ 2, 32 GET m.o_drives DEFAULT 1  ;
  SIZE 3, 12 FROM a_drives  ;
  PICTURE '@^' VALID _qh30vsudn()  ;
  COLOR SCHEME 5,6
@ 5, 32 GET m.o_dirs DEFAULT 1  ;
  SIZE 3, 12 FROM a_dirs PICTURE  ;
  '@^' VALID _qh30vsuj4() COLOR  ;
  SCHEME 5,6
@ 9, 32 GET m.h_exitcode DEFAULT  ;
  1 SIZE 1, 11, 1 PICTURE  ;
  '@*VT \!\<Abrir;\<Nuevo..;\?\<Cancela'
@ 6, 21 SAY 'Directorio'
IF  .NOT. WVISIBLE('_qh30vsr09')
     ACTIVATE WINDOW _qh30vsr09
ENDIF
READ CYCLE SHOW _qh30vsuqc()  ;
     ACTIVATE _qh30vsuq9()
RELEASE WINDOW _qh30vsr09
SELECT (m.currarea)
IF m.talkstat = 'ON'
     SET TALK ON
ENDIF
IF m.compstat = 'ON'
     SET COMPATIBLE ON
ENDIF
SET TALK OFF
m.skeleton = ''
DO CASE
     CASE m.h_exitcode = 1
          FOR m.i = 1 TO  ;
              ALEN(a_dirs, 1)
               m.skeleton = m.skeleton +  ;
                            a_dirs(m.i, ;
                            1) +  ;
                            '\'
          ENDFOR
          m.skeleton = m.skeleton +  ;
                       a_files(m.l_files, ;
                       1)
     CASE m.h_exitcode = 2
          m.skeleton = 'NEW FILE'
     CASE m.h_exitcode = 3
ENDCASE
SET DEFAULT TO (LEFT(m.savepath, 2))
SET DEFAULT TO (LEFT(m.savepath, RAT('\',;
m.savepath)))
IF m.talkstat = 'ON'
     SET TALK ON
ENDIF
RETURN m.skeleton
*
PROCEDURE iarray
PARAMETER withwhat, opt1, opt2
DO CASE
     CASE UPPER(withwhat) ==  ;
          'DRIVES'
          DIMENSION a_drives[ 1,  ;
                    1]
          m.default = SET('default')
          m.onerror = ON('error')
          ON ERROR m.error=.t.
          m.error = .F.
          m.j = 0
          FOR m.i = 1 TO  ;
              m.numdrive
               m.x = CHR(64 +  ;
                     m.i) + ':'
               SET DEFAULT TO (m.x)
               IF m.error
                    m.error = .F.
               ELSE
                    m.j = m.j + 1
                    DIMENSION a_drives[  ;
                              m.j,  ;
                              1]
                    a_drives[ m.j,  ;
                            1] =  ;
                            m.x
               ENDIF
          ENDFOR
          on error &onerror
          SET DEFAULT TO (m.default)
     CASE UPPER(m.withwhat) =  ;
          'FILES'
          DIMENSION a_files[ 1,  ;
                    1]
          a_files = .F.
          m.tskeleton = IIF(  ;
                        .NOT.  ;
                        EMPTY(m.opt1),  ;
                        STRTRAN(m.opt1,  ;
                        ' '),  ;
                        '*.*')
          m.tskeleton = IIF(m.tskeleton ==  ;
                        ';', '*.',  ;
                        m.tskeleton)
          m.dirs = IIF(m.opt2  ;
                   .OR.  ;
                   PARAMETERS() <  ;
                   4, .T., .F.)
          DIMENSION x[ 1, 1]
          DO WHILE  .NOT.  ;
             EMPTY(m.tskeleton)
               DO CASE
                    CASE m.dirs
                         m.mask =  ;
                          ''
                    CASE ';' $  ;
                         m.tskeleton
                         m.mask =  ;
                          LEFT(m.tskeleton,  ;
                          AT(';',  ;
                          m.tskeleton) -  ;
                          1)
                         m.tskeleton =  ;
                          SUBSTR(m.tskeleton,  ;
                          AT(';',  ;
                          m.tskeleton) +  ;
                          1)
                         m.tskeleton =  ;
                          STRTRAN(m.tskeleton,  ;
                          m.mask +  ;
                          ';')
                    CASE '|' $  ;
                         m.tskeleton
                         m.mask =  ;
                          LEFT(m.tskeleton,  ;
                          AT('|',  ;
                          m.tskeleton) -  ;
                          1)
                         m.tskeleton =  ;
                          SUBSTR(m.tskeleton,  ;
                          AT('|',  ;
                          m.tskeleton) +  ;
                          1)
                         m.tskeleton =  ;
                          STRTRAN(m.tskeleton,  ;
                          m.mask +  ;
                          '|')
                    OTHERWISE
                         m.mask =  ;
                          m.tskeleton
                         m.tskeleton =  ;
                          ''
               ENDCASE
               IF LEN(m.mask) = 3  ;
                  .AND.  .NOT.  ;
                  '*' $ m.mask  ;
                  .AND.  .NOT.  ;
                  '?' $ m.mask  ;
                  .AND.  .NOT.  ;
                  '.' $ m.mask
                    m.mask = '*.' +  ;
                             m.mask
               ENDIF
               IF m.dirs
                    m.dirs = .F.
                    m.hits = ADIR(x,  ;
                             m.mask,  ;
                             'D')
               ELSE
                    m.hits = ADIR(x,  ;
                             m.mask)
               ENDIF
               IF  .NOT.  ;
                   EMPTY(m.hits)
                    IF EMPTY(a_files(1, ;
                       1))
                         m.offset =  ;
                          0
                         DIMENSION  ;
                          a_files[  ;
                          ALEN(x,  ;
                          1), 1]
                    ELSE
                         m.offset =  ;
                          ALEN(a_files,  ;
                          1)
                         DIMENSION  ;
                          a_files[  ;
                          ALEN(a_files,  ;
                          1) +  ;
                          ALEN(x,  ;
                          1), 1]
                    ENDIF
                    FOR m.i = 1  ;
                        TO ALEN(x,  ;
                        1)
                         a_files[  ;
                                m.offset +  ;
                                m.i,  ;
                                1] =  ;
                                IIF('D' $  ;
                                x(m.i, ;
                                5),  ;
                                CHR(1),  ;
                                '') +  ;
                                x(m.i, ;
                                1)
                    ENDFOR
               ENDIF
          ENDDO
          IF  .NOT.  ;
              EMPTY(a_files(1, ;
              1))
               IF ASORT(a_files) <>  ;
                  1
                    ?? CHR(7)
                    WAIT WINDOW  ;
                         'Unsuccessful sort!'
               ENDIF
               FOR m.i = 1 TO  ;
                   ALEN(a_files,  ;
                   1)
                    IF ASC(a_files(m.i, ;
                       1)) = 1
                         a_files[  ;
                                m.i] =  ;
                                '[' +  ;
                                SUBSTR(a_files(m.i, ;
                                1),  ;
                                2) +  ;
                                ']'
                    ELSE
                         EXIT
                    ENDIF
               ENDFOR
          ELSE
               a_files = ''
          ENDIF
     CASE UPPER(withwhat) ==  ;
          'PATH'
          DIMENSION a_dirs[ 1, 1]
          a_dirs[ 1, 1] =  ;
                SET('default')
          x = SUBSTR(CURDIR(), 2)
          DO WHILE  .NOT.  ;
             EMPTY(x)
               DIMENSION a_dirs[  ;
                         ALEN(a_dirs,  ;
                         1) + 1,  ;
                         1]
               a_dirs[  ;
                     ALEN(a_dirs,  ;
                     1), 1] =  ;
                     LEFT(x,  ;
                     AT('\', x) -  ;
                     1)
               x = SUBSTR(x,  ;
                   AT('\', x) +  ;
                   1)
          ENDDO
ENDCASE
RETURN
*
PROCEDURE _qh30vstwl
IF m.l_files = 0
     SHOW GET m.h_exitcode, 1  ;
          DISABLE
ENDIF
IF LEFT(a_files(m.l_files,1), 1) =  ;
   '['
     SHOW GET m.h_exitcode, 1  ;
          DISABLE
ELSE
     SHOW GET m.h_exitcode, 1  ;
          ENABLE
ENDIF
*
PROCEDURE _qh30vsu0l
IF LEFT(a_files(m.l_files,1), 1) =  ;
   '['
     m.fromdir = CURDIR()
     IF a_files(m.l_files,1) ==  ;
        '[.]'
          m.fromdir = SUBSTR(m.fromdir,  ;
                      2)
          m.fromdir = '[' +  ;
                      LEFT(m.fromdir,  ;
                      AT('\',  ;
                      m.fromdir) -  ;
                      1) + ']'
          SET DEFAULT TO (m.o_drives +;
'\')
     ELSE
          IF a_files(m.l_files,1) ==  ;
             '[..]'
               m.fromdir = SUBSTR(m.fromdir,  ;
                           RAT('\',  ;
                           m.fromdir,  ;
                           2) +  ;
                           1)
               m.fromdir = '[' +  ;
                           LEFT(m.fromdir,  ;
                           LEN(m.fromdir) -  ;
                           1) +  ;
                           ']'
          ELSE
               m.fromdir = '[..]'
          ENDIF
          m.dir = ''
          FOR m.i = 1 TO m.o_dirs -  ;
              1
               m.dir = m.dir +  ;
                       IIF( .NOT.  ;
                       EMPTY(m.dir),  ;
                       '\', '') +  ;
                       a_dirs(m.i, ;
                       1)
          ENDFOR
          m.dir = m.dir + IIF(  ;
                  .NOT.  ;
                  EMPTY(m.dir),  ;
                  '\', '')
          IF a_files(m.l_files,1) ==  ;
             '[..]'
               SET DEFAULT TO (m.dir)
          ELSE
               SET DEFAULT TO (m.dir +;
a_dirs(ALEN(a_dirs, 1),1) + '\' + STRTRAN(STRTRAN(a_files(m.l_files,1),;
'['), ']') + '\')
          ENDIF
     ENDIF
     DO iarray WITH 'path'
     m.o_dirs = ALEN(a_dirs, 1)
     DO iarray WITH 'files',  ;
        IIF(m.k_allfiles, '*.*',  ;
        m.skeleton)
     m.l_files = ASCAN(a_files,  ;
                 m.fromdir)
     _CUROBJ = OBJNUM(m.l_files)
ELSE
     _CUROBJ = OBJNUM(m.h_exitcode)
     KEYBOARD CHR(13) PLAIN
ENDIF
SHOW GETS
*
PROCEDURE _qh30vsu9k
DO iarray WITH 'files',  ;
   IIF(m.k_allfiles, '*.*',  ;
   m.skeleton), m.chdir
SHOW GETS
*
FUNCTION _qh30vsudn
IF  .NOT. EMPTY(m.isdiskin)
     m.drive = m.o_drives
     CALL isdiskin WITH m.drive
     IF m.drive = '0:'
          ?? CHR(7)
          WAIT WINDOW NOWAIT  ;
               'Drive ' +  ;
               m.o_drives +  ;
               ' no est  listo!'
          RETURN 0
     ENDIF
ENDIF
SET DEFAULT TO (m.o_drives + CURDIR(m.o_drives))
DO iarray WITH 'path'
m.o_dirs = ALEN(a_dirs, 1)
DO iarray WITH 'files',  ;
   IIF(m.k_allfiles, '*.*',  ;
   m.skeleton), m.chdir
m.l_files = IIF( .NOT.  ;
            EMPTY(a_files(1,1)),  ;
            1, 0)
_CUROBJ = OBJNUM(m.l_files)
SHOW GETS
*
PROCEDURE _qh30vsuj4
m.dir = ''
FOR m.i = 1 TO m.o_dirs
     m.dir = m.dir + IIF( .NOT.  ;
             EMPTY(m.dir), '\',  ;
             '') + a_dirs(m.i,1)
ENDFOR
SET DEFAULT TO (m.dir + '\')
DO iarray WITH 'path'
m.o_dirs = ALEN(a_dirs, 1)
DO iarray WITH 'files',  ;
   IIF(m.k_allfiles, '*.*',  ;
   m.skeleton), m.chdir
m.l_files = IIF( .NOT.  ;
            EMPTY(a_files(1,1)),  ;
            1, 0)
_CUROBJ = OBJNUM(m.l_files)
SHOW GETS
*
PROCEDURE _qh30vsuq9
IF  .NOT. m.chdrive
     SHOW GET m.o_drives DISABLE
ENDIF
IF  .NOT. m.chdir
     SHOW GET m.o_dirs DISABLE
ENDIF
IF  .NOT. m.allfiles
     SHOW GET m.k_allfiles  ;
          DISABLE
ENDIF
IF  .NOT. m.newfile
     SHOW GET m.h_exitcode, 2  ;
          DISABLE
ENDIF
IF m.skeleton == '*.*' .OR.  ;
   EMPTY(m.skeleton)
     m.k_allfiles = .T.
     SHOW GET m.k_allfiles  ;
          DISABLE
ENDIF
IF m.l_files = 0
     SHOW GET m.h_exitcode, 1  ;
          DISABLE
ENDIF
*
PROCEDURE _qh30vsuqc
PRIVATE currwind
STORE WOUTPUT() TO currwind
IF EMPTY(a_files(1,1)) .OR.  ;
   m.l_files = 0
     SHOW GET m.l_files DISABLE
ELSE
     SHOW GET m.l_files ENABLE
ENDIF
IF SYS(2016) = '_QH30VSR09' .OR.  ;
   SYS(2016) = '*'
     ACTIVATE WINDOW SAME  ;
              _qh30vsr09
     @ 0, 3 SAY m.prompt SIZE 1,  ;
       40
ENDIF
IF  .NOT. EMPTY(currwind)
     ACTIVATE WINDOW SAME  ;
              (currwind)
ENDIF
*
