!  Read_DIVA.f90 
!
!  FUNCTIONS: Bround
!  Read_DIVA - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Read_DIVA
!  
! READ DIVA OUTPUT DATA, CALCULATE THE MEAN TEMPERATURE AND THE RANGE FOR EACH MONTH
! OUTPUT THE DATA IN THE FORMAT REQUIRED FOR MARKSIM AS SEPARATE *.DAT FILES AND
! WRITE THE BATCH FILES TO RUN MARKSIM FOR THE WHOLE SET OF DATA.
!
! Read_DIVA EXPECTS THE FIRST FIELD TO BE THE SITE NAME, FOLLOWED BY LATITUDE AND LONGITUDE
! (FROM THE INPUT FILE). THEN FOLLOWS THE NORMAL DIVA OUTPUT:
! RecNo PointNo Lon_ext Lat_ext alt (tminx tmax precx), x=1,12
! ALL FIELDS ARE SEPARATED BY THE TAB CHARACTER (09h)
!
! Outputs are the *.DAT files, the .CBF, and .XBF files for input to MarkSim.
! The .CBF file has the format DAT_file_path/DAT_file_name.DAT. The .CLX files that it outputs
! get their names from the first A8 field in each .DAT file, NOT from DAT_file_name.
! The .XBF file has the format CLX_file_path/CLX_file_name.CLX, WSTN (A4), random#_seed, NYRS,
! and d (for DSSAT output)/ c (for calendar output). The entries in each line are comma delimited,
! and must be present or MarkSim will crash. The random# seed can be a null entry.
!
!****************************************************************************

    program Read_DIVA

    implicit none

! Variables

    integer i,k,alt,recno,precdata(12),j, colskip, novars, kount, lenhead, nextstart, nochars
    integer kchar(10), prefix
    real latlong(2), tempdata(24), dum(4), temprange(12), meantemp(12), bround, avtemp
    character*8 head(100)
    character*4 filler
    character*512 path
    character*500 thead
    character*50 DIVA_File, name
    character*1 headchar, divaspace
    data filler/'0000'/
    logical foundlat /.false./, revcoords /.false./, tempfound /.true./

! Body of Read_DIVA

! Open the DIVA output file, read its header and discard. We do some rudimentary checking, primarily to
! make sure that latitude precedes longitude and the location of the first data field (tmin1).

    call GetPath(path)
    
!    write(*,'(a)')"Enter the name of the DIVA output file."
!    read(*,'(a)')DIVA_File
!    Diva_File='diva_salida.txt'

    open(unit=11, file=path(1:len_trim(path)),status='old')
    read(11,'(a)')thead
    path=path(1:index(path,"\",.TRUE.))

! Find out how many fields there are and how many there are before the actual data start by decoding thead.
! Note that the field separator in DIVA output in NOT a space (ASCII 32) but a tab (ASCII 09).

    novars=0
    k=0
    nextstart=1
    Divaspace=char(09)   
    do i=1,len_trim(thead)
        if(thead(i:i).eq.divaspace) then
            k=k+1
            head(k)=thead(nextstart:i-1)
            nextstart=i+1
            if(head(k)(1:5).eq.'tmin1'.and.tempfound) then
                colskip=k-1
                tempfound=.false.
            end if
        end if
    end do

! Convert any of the first headings (where latitiude and longitude should be) into upper case.

    do i=1,colskip
        nochars=len_trim(head(i))
        do k=1,nochars
            headchar=head(i)(k:k)
            kchar(k)=ichar(headchar)
            if(kchar(k).ge.97) head(i)(k:k)=char(kchar(k)-32)
        end do

! Check that latitude precedes latitude. If the order is reversed, MarkSim will likely crash and if it does not
! the data it generates will be for somewhere else. Provide a diagnostic and reverse the order on output
! [revcoords=.true.]

        if(head(i)(1:3).eq.'LAT'.and..not.foundlat) foundlat=.true.
        if(head(i)(1:3).eq.'LON'.and..not.foundlat) then
            revcoords=.true.
            write(*,'(a,/,a)')'LONGITUDE PRECEDES LATITUDE, WHICH WILL CAUSE MARKSIM TO CRASH!!', &
                'ORDER REVERSED ON OUTPUT.'
        end if
    end do
            
! Open files to receive the process log, the lines for the .CBF and .XBF files for input to MarkSim.

    open(unit=13,status='unknown',file=path(1:len_trim(path))//'ReadDIVA.log')
    open(unit=14,status='unknown',file=path(1:len_trim(path))//'ReadDIVA.CBF')
    open(unit=15,status='unknown',file=path(1:len_trim(path))//'ReadDIVA.XBF')
    do while (.not.eof(11))

! Read the data a line at a time, in the order Tmin, Tmax, Prec grouped by months. Note Prec is integer,
! Tmin and Tmax are reals. 

        read(11,*)name,(latlong(i),i=1,2),recno,(dum(i),i=1,colskip-5),alt,((tempdata(i),tempdata(i+12), &
            precdata(i)),i=1,12)

! For DIVA, numeric site names are numbers not  padded with blanks, although we read them as characters.
! To create four character site names, xxyy, where both xx and yy are 00-99, we have to pad the names 
! with zeros where name is less than 4 characters. Where there are more than 9999, we have to use letters in the
! first position. Maybe we should do that for all four characters in the future. The maximum number of sites that
! can be processed with this scheme is (9+26)*1000=35,000.

        j=len_trim(name)
        if(j.lt.4) then
            name(1:4)=filler(1:4-j)//name(1:j)
        else
            if(j.gt.8)name=name(1:8)
        end if

! Calculate temperature range and mean, use bankers' rounding (FUNCTION BROUND) on the mean to avoid bias.

        do i=1,12
            temprange(i)=tempdata(i+12)-tempdata(i)
            avtemp=(tempdata(i)+tempdata(i+12))/2
            meantemp(i)=bround(avtemp,10.0)
        end do

! Open the .dat file and write the data to it in MarkSim format and close it. 

        open(unit=12,status='unknown',file=path(1:len_trim(path))//name(1:len_trim(name))//'.dat')
        if(.not.revcoords) then

! Reverse coordinates if necessary i.e. revcoords is .true.

            write(12,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(1),latlong(2),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        else
            write(12,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(2),latlong(1),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        end if   
        close(12)

!  Write the log, the .CBF and .XBF entries and to the screen.

        write(13,'(2a/)')'MarkSim climate data written to file ',path(1:len_trim(path))// &
            name(1:len_trim(name))//'.dat'

! Reverse coordinates if necessary i.e. revcoords is .true.

        if(.not.revcoords) then
            write(13,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(1),latlong(2),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        else
            write(13,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(2),latlong(1),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        end if   
        write(14,'(a)')path(1:len_trim(path))//name(1:len_trim(name))//'.dat'

! Limit the name to four characters. It might be worthwhile to see if the truncated name gives duplicates.
! A bit difficult to do programatically unless the existing weather data are in the same folder. Of course,
! you could ask for the folder name, and check there.

        write(15,'(3a)')path(1:len_trim(path))//'OUTPUT\'//name(1:len_trim(name))//'.CLX,',name(1:4), &
            ',1234,99,d'
        write(*,'(2a/)')'MarkSim climate data written to file ',path(1:len_trim(path))// &
            name(1:len_trim(name))//'.dat'

! Reverse coordinates if necessary i.e. revcoords is .true.

        if(.not.revcoords) then
            write(*,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(1),latlong(2),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        else
            write(*,'(a8,2f8.4,i6/12i5/2(12f5.1/))')name,latlong(2),latlong(1),alt,(precdata(i),i=1,12), &
                (meantemp(i),i=1,12),(temprange(i),i=1,12)
        end if   
    end do
    
! Close the log, .CBF and .XBF files and write a closing message.

    close(13)
    close(14)
    close(15)
    write(*,'(a)')'End of file on DIVA input file. Closing files.'
    end program Read_DIVA

    FUNCTION BROUND (X, FACTOR)

! IMPLEMENTS BANKERS' ROUNDING. FOR DATA <0.5 ROUNDS DOWN,
! >0.5 ROUNDS UP, =0.5 ROUNDS TO EVEN DIGIT

    real bround, x, factor,temp, ksgn
    real fixtemp
      TEMP=X*FACTOR

! SIGN(y,x) transfers the sign of x to y, so that ksgn is either 1.0 or -1.0 depending whether
! x positive or negative

      ksgn=sign(1.0,x)
      FIXTEMP=INT(TEMP+0.5*ksgn)

! HANDLE ROUNDING OF 0.5 IN A SPECIAL MANNER
! Cecks to see if the remainder is 0.5 or -0.5 (note that the left hand term is not the same as (mod(x,1))

      IF(TEMP-INT(TEMP).EQ.ksgn*0.5) THEN
        IF(FIXTEMP/2.NE.INT(FIXTEMP/2)) THEN
!
! TEMP IS ODD, REDUCE IT BY 1 TO MAKE IT EVEN
!
          FIXTEMP=FIXTEMP-SIGN(1.0,X)
        ENDIF
      ENDIF
      BROUND=FIXTEMP/FACTOR
      END FUNCTION BROUND

    subroutine GetPath (file_spec)
    
! Uses the Windows API to return the full path and filename.
! Note that it does this in a console, not the full Windows environment.

    use ifwin
    use comdlg32
    use user32 ! Interface for GetForegroundWindow
    implicit none

! Declare structure used to pass and receive attributes

    type(T_OPENFILENAME) ofn

! Declare filter specification.  This is a concatenation of pairs of null-terminated strings.
! The first string in each pair is the file type name, the second is a semicolon-separated list
! of file types for the given name.  The list ends with a trailing null-terminated empty string.

    character(*),parameter :: filter_spec = &
      "Text Files"C//"*.txt"C// &
      "All Files"C//"*.*"C//""C

! Declare string variable to return the file specification.
! Initialize with an initial filespec, if any - null string
! otherwise
! This line gave a compiler error, I guess because file_spec is an arguement in the subroutine definition.
!
! character(512) :: file_spec = ""C

    character(512) file_spec
    integer status,ilen
        ofn%lStructSize = SIZEOF(ofn)
        ofn%hwndOwner = GetForegroundWindow()
        ofn%hInstance = NULL  ! For Win32 applications, you can set this to the appropriate hInstance
        ofn%lpstrFilter = loc(filter_spec)
        ofn%lpstrCustomFilter = NULL
        ofn%nMaxCustFilter = 0
        ofn%nFilterIndex = 1 ! Specifies initial filter value
        ofn%lpstrFile = loc(file_spec)
        ofn%nMaxFile = sizeof(file_spec)
        ofn%nMaxFileTitle = 0
        ofn%lpstrInitialDir = NULL  ! Use Windows default directory
        ofn%lpstrTitle = loc(""C)
        ofn%Flags = OFN_PATHMUSTEXIST
        ofn%lpstrDefExt = loc("txt"C)
        ofn%lpfnHook = NULL
        ofn%lpTemplateName = NULL

    file_spec = ""C

! Call GetOpenFileName and check status

    status = GetOpenFileName(ofn)
    if (status .eq. 0) then
        type *,'No file name specified'
    else
    
        ! Get length of file_spec by looking for trailing NUL
        
        ilen = INDEX(file_spec,CHAR(0))
        type *,'Filespec is ',file_spec(1:ilen-1)
        
        ! Example of how to see if user said "Read Only"

        if (IAND(ofn%flags,OFN_READONLY) /= 0) &
        type *,'Readonly was requested'
    end if
    end subroutine GetPath