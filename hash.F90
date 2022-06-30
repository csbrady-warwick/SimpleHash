MODULE hash_mod
        USE ISO_FORTRAN_ENV
        IMPLICIT NONE
contains
FUNCTION hash(key, ne)
        !Implementation of the DJB2 hashing algorithm
        CHARACTER(LEN=*), INTENT(IN) :: key
        INTEGER, INTENT(IN) :: ne
        INTEGER(INT64) :: hash
        INTEGER :: i

        hash = 5381_INT64
        DO i = 1, LEN_TRIM(key)
          !ISHFT(hash,5) + hash === hash * 33
          hash = MOD((ISHFT(hash, 5) + hash) + INT(IACHAR(key(i:i)), INT64), ne)
        END DO

        hash = hash + 1
END FUNCTION hash

END MODULE hash_mod

!This is a very simple implementation of a hash table
!It doesn't do anything with hash collisions and has no buckets
!But it shows the principle of using a hashing algorithm to map
!from a key to an index in an array
PROGRAM test
   USE hash_mod
   IMPLICIT NONE
   INTEGER,DIMENSION(100) :: data

   data(hash("Hello",100)) = 16
   data(hash("World",100)) = 24
   data(hash("There",100)) = 99

   PRINT *, data(hash("Hello",100)), data(hash("World",100)), data(hash("There",100))

END PROGRAM test
