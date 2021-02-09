      subroutine foo
      integer word, wrap_after_comma, wrap_after_right_paren
      logical accessor, TEST, LOGICAL2VAR
      if (accessor(word, wrap_after_comma)) call bar
      if (accessor(int(word), wrap_after_comma)) call bar
      if (accessor(int(int2(word)), wrap_after_comma)) call bar
      if (accessor(int(int2(word)))) call wrap_after_right_paren
      if (accessor(word)) call foo(bar)!don't wrap inline comment
      accessor(1) = TEST!inline comment not to be wrapped

      if(1) then
            LOGICAL2VAR = l2!other comment that shouldn't be wrapped
            !comments after 0! char replacements don't wrap!
            L!comments after replacements >0 chars don't wrap
      endif
      end
