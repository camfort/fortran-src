module testing
  interface named
     subroutine foo(arg)
       real :: arg
     end subroutine foo
  end interface named !breaks our parser
end module testing
