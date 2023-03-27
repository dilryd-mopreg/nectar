/+  n=nectar
|%
++  name  %shared-table
+$  rock  table:n
+$  wave  query:n
++  wash
  |=  [=table:n =query:n]
  ?+    -.query  table  ::  no-op
      %update
    +:(~(update tab:n table) primary-key.table where.query cols.query)
      %insert
    =-  (~(insert tab:n table) - update=%.n)
    `(list row:n)`(turn rows.query |=(i=* !<(row:n [-:!>(*row:n) i])))
      %delete
    ::  TODO intelligent selection here
    =/  query-key  primary-key.table
    (~(delete tab:n table) query-key where.query)
      %update-rows
    =-  (~(insert tab:n table) - update=%.y)
    `(list row:n)`(turn rows.query |=(i=* !<(row:n [-:!>(*row:n) i])))
      %add-column
    (~(add-column tab:n table) +.+.query)
      %drop-column
    (~(drop-column tab:n table) col-name.query)
      %edit-column
    (~(edit-column tab:n table) +.+.query)
  ==
--