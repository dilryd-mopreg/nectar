/+  n=nectar
|%
::  :nectar &nectar-query %my-app^[%add-table %my-table [(malt ~[[%id [0 %.n %ud]]]) ~[%id] (malt ~[[~[%id] [~[%id] %.y ~ %.y %.y]]]) ~]]
::
::  :nectar &nectar-query %my-app^[%drop-table %my-table]
::
::  :nectar &nectar-set-perms [%my-app %my-table]^[%public ~]
::
::  :nectar &nectar-track %my-app^[%start ~nec %my-app^%my-table]
::
::  :nectar &nectar-query %my-app^[%insert %my-table ~[[1 ~] [2 ~] [3 ~]]]
::
++  name  %shared-table
+$  rock  table:n
+$  wave  query:n
++  wash
  |=  [=rock =wave]
  ?+    -.wave  rock  ::  no-op
      %add-table  ::  happens at creation, once
    (~(create tab:n actual.wave) ~)
      %update
    +:(~(update tab:n rock) primary-key.rock where.wave cols.wave)
      %insert
    =-  (~(insert tab:n rock) - update=%.n)
    `(list row:n)`(turn rows.wave |=(i=* !<(row:n [-:!>(*row:n) i])))
      %delete
    ::  TODO intelligent selection here
    =/  query-key  primary-key.rock
    (~(delete tab:n rock) query-key where.wave)
      %update-rows
    =-  (~(insert tab:n rock) - update=%.y)
    `(list row:n)`(turn rows.wave |=(i=* !<(row:n [-:!>(*row:n) i])))
      %add-column
    (~(add-column tab:n rock) +.+.wave)
      %drop-column
    (~(drop-column tab:n rock) col-name.wave)
      %edit-column
    (~(edit-column tab:n rock) +.+.wave)
  ==
--