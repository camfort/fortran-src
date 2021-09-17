      REAL*8 FUNCTION foo_removals ()

      real*8            sec_rate(31),cur_rate(31),day_value,
     .                  month_rec_values(5,31),in_values(5),
     .                  blank(31)

      integer*4 somevalue1, somevalue2

                 if                (my_fn_call(1) .or. deadcode(2)
     .        .and.    my_fn_call2(somevalue1) .and. my_fn_call3(10))
     .            then
                 call process_things(1,2,3)
             endif

      foo_removals = 0

      return
      end
