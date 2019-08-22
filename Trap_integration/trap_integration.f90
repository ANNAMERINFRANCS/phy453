program Trapazoidal
      implicit none
      real*8:: h,x0,xn,Area,x,error,f0,fn,func
      integer:: n,i

      x0=0.0d0
      xn=1.0d0
      f0=func(0.0d0)
      fn=func(1.0d0)
      n=100
      error=100
      
      do while (abs(error)>1e-10)
        n=n+100
        h=(xn-x0)/n
        Area=h*(f0+fn)/2
        
        do i=1,n-1
                x=x0+h*i
                Area=Area+func(x)*h
        end do

        error=4.0d0*atan(1.0d0)-Area
        print*, n, Area, error
      
      end do
        



end program Trapazoidal


function func(x) result(y)
      real*8::x,y
      y=(4/1+x**2)
end function func
