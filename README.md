esql is a package that try to augment the usefullness of <code>sql.el</code>.

<code>M-x esql-scratchpad</code> start to a file connected to a sql buffer it
use <code>sql-connection-alist</code>.

<code>M-x esql-send-request</code> allow to use parameter in comments before
sending the request to the sqli buffer.

```sql
-- :author 'Jules Verne'
-- :date '1870-01-01'
SELECT *
FROM book
WHERE author = :author
AND published_at > :date
```
You can also use elisp.

```sql
-- :author ,(concat "'" (downcase "Jules Verne") "'")
-- :date '1870-01-01'
SELECT *
FROM book
WHERE author = :author
AND published_at > :date
```

Completion for table name, functions, types and keywords

TODO
TODO complete schema
