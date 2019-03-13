# ZCL_REGEX
ABAP-Klasse zur objektorientierten Kapselung für Reguläre Ausdrücke.

## Aufbau eines Regulären Ausdruckes

```abap
DATA(lv_regex) = '/<Regex>/gi'.
```
Der Regex-String, welcher in jede Methode der Klasse ZCL_REGEX gegeben wird, ist wie ein Regex aus JavaScript aufgebaut. Dies heißt das der Regex zwischen die '/' geschrieben wird, und am Ende Paremeter noch mitgegeben werden können.
* i : ignoring case (nicht Casesensitiv)
* g : global (Suche findet global statt)

## ZCL_REGEX=>MATCH

```abap
zcl_regex=>matches( iv_val   = 'ABAP'
                    iv_regex =  '/a/ig' ).
```
Dieser Aufruf gibt ABAP_TRUE zurück.

```abap
zcl_regex=>matches( iv_val   = 'ABAP'
                    iv_regex =  '/a/g' ).
```
Dieser Aufruf gibt ABAP_FALSE zurück, da dieser Aufruf Casesensitiv ist.

## ZCL_REGEX=>MATCHES

```abap

```

## ZCL_REGEX=>SPLIT

```abap

```
