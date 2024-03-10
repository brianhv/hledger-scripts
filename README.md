# hledger scripts

This is an unpolished place to put my scripts for hledger.

Right now there's just one script, `hledger-txnsbycat`. This produces a report
of income and expense transactions matching the provided query, grouped by
category.

```
$ ./hledger-txnsbycat.hs -V -p "last month" "Expenses:Computer" "Expenses:Entertainment"
root
   Expenses
      Expenses:Computer
            2024-02-01  VPS for personal website                     $10.00
            2024-02-22  Backup object storage                         $1.29
            2024-02-27  Ergonomic mechanical keyboard               $365.00
                                     Total for Expenses:Computer    $376.29
      Expenses:Entertainment
            2024-02-01  Paramount+                                    $1.98
            2024-02-16  Humble Bundle                                $25.00
            2024-02-19  Dropout                                      $47.99
            2024-02-28  YouTube Premium                              $14.83
         Expenses:Entertainment:Music
            2024-02-10  Spotify                                      $10.99
                          Total for Expenses:Entertainment:Music     $10.99
                                Total for Expenses:Entertainment    $100.79
                                              Total for Expenses    $477.08
                                                           Total    $477.08
```
