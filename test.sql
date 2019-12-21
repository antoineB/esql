select toto
from tutu
where id = 1;

/* toto */

-- :tutu 12
-- :id 24
with toto (select toto
           from tutu
           where toto = 12)
/** tptp */
select tutu, 'toto'
from ja as j
  left join "ja\"aaaaa" AS j ON toto = 12
where toto = :toto
and tutu = @tutu
and koo = ?
and poo = $1
group by toto;

