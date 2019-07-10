class ZCL_REGEX definition
  public
  final
  create public .

public section.

    "! <p class="shorttext synchronized">Konvertiert einen ABAP Regex in Passendes format</p>
  class-methods CONVERT_ABAP_REGEX
    importing
      !IV_ABAP_REGEX type STRING
      !IV_GLOBAL_SEARCH type ABAP_BOOL default ABAP_FALSE
      !IV_IGNORE_CASE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_REGEX) type STRING
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
    "! <p class="shorttext synchronized">Prüft ob String den Regex enthält</p>
  class-methods MATCH
    importing
      !IV_VAL type STRING
      !IV_REGEX type STRING
    returning
      value(RV_CONTAINS) type ABAP_BOOL
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
    "! <p class="shorttext synchronized">Gibt die Matches des Regex zurück</p>
  class-methods MATCHES
    importing
      !IV_VAL type STRING
      !IV_REGEX type STRING
    returning
      value(RT_MATCHES) type MATCH_RESULT_TAB
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
    "! <p class="shorttext synchronized">Gibt die Matches des Regex als String zurück</p>
  class-methods MATCHES_AS_STRING
    importing
      !IV_VAL type STRING
      !IV_REGEX type STRING
    returning
      value(RT_STRINGS) type STRINGTAB
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
    "! <p class="shorttext synchronized">Ersetzt einen Regex durch den angeegebenen String</p>
  class-methods REPLACE
    importing
      !IV_VAL type STRING
      !IV_REGEX type STRING
      !IV_REPLACE type STRING
    returning
      value(RV_VAL) type STRING
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
    "! <p class="shorttext synchronized">Splitted einen String an Regex auf</p>
  class-methods SPLIT
    importing
      !IV_VAL type STRING
      !IV_REGEX type STRING
    returning
      value(RT_SPLIT) type STRINGTAB
    raising
      CX_SY_REGEX_TOO_COMPLEX
      CX_SY_REGEX .
  PROTECTED SECTION.
private section.

  class-data S_SPLIT_VAL type STRING value '69b03f5a-457b-11e9-b210-d663bd873d93-499aba64-457c-11e9-b210-d663bd873d93-51a0e134-457c-11e9-b210-d663bd873d93' ##NO_TEXT.
  constants C_GLOBAL type STRING value 'g' ##NO_TEXT.
  constants C_IGNORE_CASE type STRING value 'i' ##NO_TEXT.
  constants C_SLASH type STRING value '/' ##NO_TEXT.

    "! Interne Nutzung
  class-methods CHECK_REGEX
    importing
      !IV_REGEX type STRING
    returning
      value(RV_OK) type ABAP_BOOL
    exceptions
      FALSE_REGEX_FROMAT .
    "! Interne Nutzung
  class-methods DETERMINE_REGEX_VALUES
    importing
      !IV_REGEX type STRING
    exporting
      !EV_REGEX type STRING
      !EV_GLOBAL type ABAP_BOOL
      !EV_IGNORE_CASE type ABAP_BOOL .
  class-methods GENERATE_NEW_SPLIT_VAL
    importing
      !IV_VAL type STRING .
ENDCLASS.



CLASS ZCL_REGEX IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_REGEX=>CHECK_REGEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REGEX                       TYPE        STRING
* | [<-()] RV_OK                          TYPE        ABAP_BOOL
* | [EXC!] FALSE_REGEX_FROMAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_regex.

    FIND REGEX '\/.*\/([ig][ig]|[ig]|$)' IN iv_regex.
    IF sy-subrc = 0.
      rv_ok = abap_true.
    ELSE.
      RAISE false_regex_fromat.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>CONVERT_ABAP_REGEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ABAP_REGEX                  TYPE        STRING
* | [--->] IV_GLOBAL_SEARCH               TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_IGNORE_CASE                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-()] RV_REGEX                       TYPE        STRING
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_abap_regex.

    " Konvertiert einen Standard ABAP Regex
    " in eine für diese Klasse verwendbare Form

    " ABAP Regex testen
    FIND REGEX iv_abap_regex IN space.

    IF sy-subrc <> 0.
      " Ok, nichts tun
    ENDIF.

    CONCATENATE c_slash iv_abap_regex c_slash INTO rv_regex.

    IF iv_ignore_case = abap_true.
      CONCATENATE rv_regex c_ignore_case INTO rv_regex.
    ENDIF.

    IF iv_global_search = abap_true.
      CONCATENATE rv_regex c_global INTO rv_regex.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_REGEX=>DETERMINE_REGEX_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REGEX                       TYPE        STRING
* | [<---] EV_REGEX                       TYPE        STRING
* | [<---] EV_GLOBAL                      TYPE        ABAP_BOOL
* | [<---] EV_IGNORE_CASE                 TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD determine_regex_values.

    CLEAR: ev_regex
         , ev_global
         , ev_ignore_case
         .

    FIND REGEX '/$' IN iv_regex.

    IF sy-subrc <> 0.

      SPLIT iv_regex AT c_slash INTO TABLE DATA(lt_split).

      DATA(lv_params) = lt_split[ lines( lt_split ) ].

      IF lv_params CS c_global.
        ev_global = abap_true.
      ENDIF.

      IF lv_params CS c_ignore_case.
        ev_ignore_case = abap_true.
      ENDIF.

      LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<ls_split>).
        IF <ls_split> IS NOT INITIAL
          AND sy-tabix <> lines( lt_split ).
          CONCATENATE ev_regex <ls_split> c_slash INTO ev_regex.
        ENDIF.
      ENDLOOP.

      FIND REGEX '.*\\\/$' IN ev_regex.
      IF sy-subrc <> 0.
        DATA(lv_offset) = strlen( ev_regex ) - 1.
        ev_regex = ev_regex+0(lv_offset).
      ENDIF.
      CONDENSE ev_regex NO-GAPS.

    ELSE.
      ev_regex = iv_regex.
      REPLACE ALL OCCURRENCES OF REGEX '(^\/|\/$)' IN ev_regex WITH ''.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_REGEX=>GENERATE_NEW_SPLIT_VAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_new_split_val.

    DO.
      FIND REGEX s_split_val IN iv_val.

      IF sy-subrc = 0.
        CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
          EXPORTING
            number_chars  = 32
          IMPORTING
            random_string = s_split_val.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>MATCH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* | [--->] IV_REGEX                       TYPE        STRING
* | [<-()] RV_CONTAINS                    TYPE        ABAP_BOOL
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD match.

    DATA: lv_global      TYPE abap_bool VALUE abap_false
        , lv_ignore_case TYPE abap_bool VALUE abap_false
        , lv_regex       TYPE string
        .

    IF abap_true = check_regex( iv_regex ).

      determine_regex_values(
        EXPORTING
          iv_regex       = iv_regex
        IMPORTING
          ev_regex       = lv_regex
          ev_global      = lv_global
          ev_ignore_case = lv_ignore_case
      ).

      IF lv_ignore_case = abap_true.
        FIND REGEX lv_regex
          IN iv_val IGNORING CASE.
      ELSE.
        FIND REGEX lv_regex
        IN iv_val RESPECTING CASE.
      ENDIF.

      IF sy-subrc = 0.
        rv_contains = abap_true.
      ELSE.
        rv_contains = abap_false.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>MATCHES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* | [--->] IV_REGEX                       TYPE        STRING
* | [<-()] RT_MATCHES                     TYPE        MATCH_RESULT_TAB
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD matches.

    IF abap_true = check_regex( iv_regex ).

      DATA: lv_global        TYPE abap_bool
          , lv_ignoring_case TYPE abap_bool
          , lv_regex         TYPE string
          .

      determine_regex_values(
        EXPORTING
          iv_regex       = iv_regex
        IMPORTING
          ev_regex       = lv_regex
          ev_global      = lv_global
          ev_ignore_case = lv_ignoring_case
      ).

      IF lv_ignoring_case = abap_true.
        IF lv_global = abap_true.
          FIND ALL OCCURRENCES OF REGEX lv_regex
            IN iv_val IGNORING CASE
            RESULTS DATA(lt_results).
        ELSE.
          FIND REGEX lv_regex
            IN iv_val IGNORING CASE
            RESULTS lt_results.
        ENDIF.
      ELSE.
        IF lv_global = abap_true.
          FIND ALL OCCURRENCES OF REGEX lv_regex
            IN iv_val RESPECTING CASE
            RESULTS lt_results.
        ELSE.
          FIND REGEX lv_regex
            IN iv_val RESPECTING CASE
            RESULTS lt_results.
        ENDIF.
      ENDIF.

      IF lt_results IS NOT INITIAL.
        rt_matches = lt_results.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>MATCHES_AS_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* | [--->] IV_REGEX                       TYPE        STRING
* | [<-()] RT_STRINGS                     TYPE        STRINGTAB
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD matches_as_string.

    DATA: lt_matches TYPE match_result_tab
        , lv_string  TYPE string
        .

    FIELD-SYMBOLS: <ls_match> TYPE match_result
                 .

    lt_matches = matches( iv_val   = iv_val
                          iv_regex = iv_regex ).

    LOOP AT lt_matches ASSIGNING <ls_match>.
      CLEAR lv_string.
      lv_string = iv_val+<ls_match>-offset(<ls_match>-length).
      APPEND lv_string TO rt_strings.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>REPLACE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* | [--->] IV_REGEX                       TYPE        STRING
* | [--->] IV_REPLACE                     TYPE        STRING
* | [<-()] RV_VAL                         TYPE        STRING
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD replace.

    DATA: lv_ignoring_case TYPE abap_bool
        , lv_global        TYPE abap_bool
        , lv_regex         TYPE string
        .

    rv_val = iv_val.

    determine_regex_values(
      EXPORTING
        iv_regex       = iv_regex
      IMPORTING
        ev_regex       = lv_regex
        ev_global      = lv_global
        ev_ignore_case = lv_ignoring_case
    ).

    IF lv_ignoring_case = abap_true.
      IF lv_global = abap_true.
        REPLACE ALL OCCURRENCES OF REGEX lv_regex
          IN rv_val
          WITH iv_replace
          IGNORING CASE.
      ELSE.
        REPLACE REGEX lv_regex
         IN rv_val
         WITH iv_replace
         IGNORING CASE.
      ENDIF.
    ELSE.
      IF lv_global = abap_true.
        REPLACE ALL OCCURRENCES OF REGEX lv_regex
          IN rv_val
          WITH iv_replace
          RESPECTING CASE.
      ELSE.
        REPLACE REGEX lv_regex
          IN rv_val
          WITH iv_replace
          RESPECTING CASE.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_REGEX=>SPLIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        STRING
* | [--->] IV_REGEX                       TYPE        STRING
* | [<-()] RT_SPLIT                       TYPE        STRINGTAB
* | [!CX!] CX_SY_REGEX_TOO_COMPLEX
* | [!CX!] CX_SY_REGEX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD split.

    DATA: lv_val TYPE string
        .

    lv_val = iv_val.

    generate_new_split_val( lv_val ).

    lv_val = replace(
      EXPORTING
        iv_val     = lv_val
        iv_regex   = iv_regex
        iv_replace = s_split_val
    ).

    SPLIT lv_val AT s_split_val INTO TABLE rt_split.
    
  ENDMETHOD.
ENDCLASS.
