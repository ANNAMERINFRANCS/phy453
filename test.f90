program test
      implicit none

      real*8 :: p(100),r(100)
      integer :: i
call random_number(p)
call random_number(r)
        write(*,*) p,r

end program test
