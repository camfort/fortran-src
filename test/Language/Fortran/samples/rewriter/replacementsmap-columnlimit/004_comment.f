c     This is the API for foo subroutine. We take in some inputs, apply some cool things and return other things.
      subroutine foo
        integer*2 a ! This is my variable 'a' ... it stores numbers both larger and small
        CHARACTER*2 some_string(15)

        some_string = "some_string"

        if (foo) then
            IF (some_string(0).eq.'s') foo=9                              Text after col 72 are comments and should remain
        endif
      end
