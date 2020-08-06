PROGRAM sort
  
  IMPLICIT NONE

  CHARACTER(len=10) :: filename
  CHARACTER(len = 40) :: msg
  
  INTEGER, PARAMETER :: LU = 8
  INTEGER :: status,nvals=0,i,iptr,j
  REAL :: temp
  INTEGER, PARAMETER :: MAXDI = 5
  REAL, DIMENSION(MAXDI) :: a

  
  WRITE(*,*) "Entrar el nombre del archivo"
  READ(*,*) filename

  OPEN(UNIT=LU,FILE=filename,IOSTAT=status,STATUS='OLD',ACTION = "READ",IOMSG=msg)

  ifopen: IF (status == 0) THEN
     WRITE(*,*) "Leio perfecto"
     DO
        READ (LU,*,IOSTAT=status) temp
        
        IF (status /= 0) EXIT
        WRITE (*,*) nvals, temp
        nvals= nvals +1
        a(nvals)=temp
     ENDDO




     primero: DO i=1, nvals-1
     iptr=i

     DO j=i+1,nvals
        IF ( a(j) < a(iptr) ) THEN
           iptr=j
        ENDIF
     ENDDO



     IF ( iptr /= i) THEN
        temp= a(i)
        a(i)= a(iptr)
        a(iptr) = temp
        
        


        
     ENDIF
        
  ENDDO primero
  
ENDIF ifopen
WRITE(*,*) a



END PROGRAM sort
