program main
    integer, parameter :: k4 = 4

    ! equivalent: STyCharacter (CharLenInt 3)
    character(3)              :: s11 = "sup"
    character(len=3)          :: s12 = "sup"
    character(len=3 , kind=1) :: s13 = "sup"
    character(kind=1, len=3 ) :: s14 = "sup"

    ! equivalent: STyCharacter (CharLenInt 1)
    character                 :: s21 = "c"
    character(1)              :: s22 = "c"
    character(len=1)          :: s23 = "c"
    character(kind=1)         :: s24 = "c"
    character(len=1 , kind=1) :: s25 = "c"
    character(kind=1, len=1 ) :: s26 = "c"

    ! equivalent: STyCharacter (CharLenInt 3)
    character    :: s31*3 = "sup"
    character(3) :: s32   = "sup"
    character(3) :: s33*3 = "sup"   ! FS soft type error (...using decl length)
    character(2) :: s34*3 = "sup"   ! FS soft type error (...using decl length)
    character(9) :: s35*3 = "sup"   ! FS soft type error (...using decl length)

    ! type error: non-CHARACTER given length
    integer      :: i11*4 = 1234

    ! special RHS len expr
    character    :: s41*(*)   = "buh?"

    ! unimplemented: unsupported RHS len expr
    character    :: s43*k4    = "uhhh"
    character    :: s44*(2*2) = "huh."
end program main
