      integer ix
      ix = 10
      ik = iz(ix)
      write (*,*) ik
      end
c
      integer function iz (iy)
      iz = iy * iy
      return
      end
