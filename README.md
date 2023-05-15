Funkcionálny projekt do predmety FLP (Funkcionálne a logické programovanie) v jazyku Haskell.




Knapsack problem

Program, který řeší optimalizační verzi 0-1 problému batohu (knapsack problem).

2.1 Rozhraní programu

Program je možné spustit:
flp22-fun volby [vstup]

kde

• vstup je jméno vstupního souboru (pokud není specifikován – stdin)

• volby jsou parametry ovlivňující chování programu:

-i ze vstupu načte informace o instanci knapsack do vaší vnitřní reprezentace. Na stdout jí vypíše zpět (očekává se, že tento výpis
bude řešen instancí typové třídy Show pro váš datový typ reprezentující knapsack).

-b ze vstupu načte informace o knapsack instanci. Na stdout vypíše
řešení nalezené prohledáváním stavového prostoru hrubou silou.
V případě, že řešení nebylo nalezeno, vypíše False.

-o ze vstupu načte informace o knapsack instanci. Na stdout vypíše
řešení nalezené pomocou optimalizační metody:
(1) genetic algorithm.  V případě, že řešení nebylo nalezeno, vypíše False.


2.2 Očekávaný vstup a výstup

V následujících ukázkách je <informace o knapsack instanci> zaměněno
za:
  
Knapsack {
  
maxWeight: 46
  
minCost: 324
  
items: [
  
Item {
weight: 36
cost: 3
}
  
Item {
weight: 43
cost: 1129
}
  
Item {
weight: 202
cost: 94
}
  
Item {
weight: 149
cost: 2084
}
  
]
  
}
  
2.2.1 -i
  
Vstup:
<informace o knapsack instanci>
Výstup:
<informace o knapsack instanci>
  
2.2.2 -b
Vstup:
<informace o knapsack instanci>
Výstup:
Solution [0 1 0 0]
  
2.2.3 -o
Stejný formát jako v případě -b.
