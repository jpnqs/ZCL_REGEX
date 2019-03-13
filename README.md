# ZCL_REGEX
ABAP-Klasse zur objektorientierten Kapselung für Reguläre Ausdrücke.

## Aufbau eines Regulären Ausdruckes

```abap
DATA(lv_regex) = '/<Regex>/gi'.
```
Der Regex-String, welcher in jede Methode der Klasse ZCL_REGEX gegeben wird, ist wie ein Regex aus JavaScript aufgebaut. Dies heißt das der Regex zwischen die '/' geschrieben wird, und am Ende Paremeter noch mitgegeben werden können.
* i : ignoring case (nicht Casesensitiv)
* g : global (Suche findet global statt)  

ABAP verwendet im Kernel die C++ Engine [Boost.Regex 5.1.1](https://www.boost.org/doc/libs/1_61_0/libs/regex/doc/html/index.html). Demnach ist mit ABAP Regex nur möglich, was diese Engine bereitstellt. Für weitere informationen über ABAP Regex klicken Sie [hier](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenregular_expressions.htm).
## ZCL_REGEX=>MATCH

```abap
IF abap_true = zcl_regex=>match( iv_val   = 'ABAP'
                                 iv_regex = '/a/ig' ).
  " Regex wurde gefunden
ELSE.
  " Regex wurde nicht gefunden
ENDIF.
```
Dieser Aufruf gibt ```abap_true``` zurück.

```abap
zcl_regex=>match( iv_val   = 'ABAP'
                  iv_regex = '/a/g' ).
```
Dieser Aufruf gibt ```abap_false``` zurück, da dieser Aufruf Casesensitiv ist.

Der Global Modifier ist für diese Methode irrelevant, da hier nur geschaut wird, ob der Regex mindestens einmal gefunden wird.

## ZCL_REGEX=>MATCHES

```abap

```

## ZCL_REGEX=>SPLIT

```abap

```
