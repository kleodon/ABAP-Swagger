class ZCL_SWAG_MAP_TYPE_JSON definition
  public
  inheriting from ZCL_SWAG_MAP_TYPE
  final
  create public .

public section.

  class ZCL_JSON definition load .
  methods CONSTRUCTOR
    importing
      !IS_PARAM type SEOSUBCODF
      !IV_SCHEMA type ABAP_BOOL default ABAP_TRUE
      !IV_PRETTY_NAME type ZCL_JSON=>PRETTY_NAME_MODE default ZCL_JSON=>PRETTY_MODE-NONE
      !IT_NAME_MAPPINGS type ZCL_JSON=>NAME_MAPPINGS optional .
protected section.

  data MV_PRETTY_NAME type ZCL_JSON=>PRETTY_NAME_MODE .
  data MT_NAME_MAPPINGS type ZCL_JSON=>NAME_MAPPINGS .

  methods MAP_STRUCTURE
    redefinition .
private section.

  methods FORMAT_NAME
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  methods PRETTY_NAME
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  methods PRETTY_NAME_EX
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
ENDCLASS.



CLASS ZCL_SWAG_MAP_TYPE_JSON IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      EXPORTING
        is_param  = is_param
        iv_schema = iv_schema
    ).

    mv_pretty_name = iv_pretty_name.
    mt_name_mappings = mt_name_mappings.
  ENDMETHOD.


  METHOD format_name.
    FIELD-SYMBOLS: <cache> LIKE LINE OF mt_name_mappings.

    CASE mv_pretty_name.
      WHEN zcl_json=>pretty_mode-camel_case.
        out = pretty_name( in ).
      WHEN zcl_json=>pretty_mode-extended.
        out = pretty_name_ex( in ).
      WHEN zcl_json=>pretty_mode-user_low_case.
        READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
        IF sy-subrc IS INITIAL.
          out = <cache>-json.
        ELSE.
          out = in.
          TRANSLATE out TO LOWER CASE.                    "#EC SYNTCHAR
        ENDIF.
      WHEN zcl_json=>pretty_mode-user.
        READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
        IF sy-subrc IS INITIAL.
          out = <cache>-json.
        ELSE.
          out = in.
        ENDIF.
      WHEN zcl_json=>pretty_mode-low_case.
        out = in.
        TRANSLATE out TO LOWER CASE.                      "#EC SYNTCHAR
      WHEN OTHERS.
        out = in.
    ENDCASE.
  ENDMETHOD.


  METHOD map_structure.
    DATA: lv_index      TYPE i,
          lv_type       TYPE string,
          lt_components TYPE cl_abap_structdescr=>component_table,
          lo_struct     TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components.


    lo_struct ?= io_typedescr.
    lt_components = lo_struct->get_components( ).

* todo, this only works with 1 level
    LOOP AT lt_components ASSIGNING <ls_component> WHERE as_include = abap_true.
      lo_struct ?= <ls_component>-type.
      APPEND LINES OF lo_struct->get_components( ) TO lt_components.
    ENDLOOP.
    DELETE lt_components WHERE as_include = abap_true.

    IF mv_schema = abap_true.
      rv_type = '"schema":{"type":"object", "properties":{'.
    ELSE.
      rv_type = '"type":"object", "properties":{'.
    ENDIF.

    LOOP AT lt_components ASSIGNING <ls_component>.
      lv_index = sy-tabix.

      ASSERT NOT <ls_component>-name IS INITIAL.

      lv_type = map_internal( <ls_component>-type ).
*      rv_type = rv_type && '"' && <ls_component>-name && '":{ ' && lv_type && ' }'.
      rv_type = rv_type && '"' && format_name( <ls_component>-name ) && '":{ ' && lv_type && ' }'.

      IF lv_index <> lines( lt_components ).
        rv_type = rv_type && ','.
      ENDIF.
    ENDLOOP.

    rv_type = rv_type && '}'.

    IF mv_schema = abap_true.
      rv_type = rv_type && '}'.
    ENDIF.
  ENDMETHOD.


  METHOD pretty_name.
    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.

      REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING `/_:_~_`.
      SPLIT out AT `_` INTO TABLE tokens.
      LOOP AT tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

      cache-abap  = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
*      INSERT cache INTO TABLE mt_name_mappings_ex.
    ENDIF.
  ENDMETHOD.


  METHOD pretty_name_ex.
    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.


      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING `/_:_~_`.

      REPLACE ALL OCCURRENCES OF `__e__` IN out WITH `!`.
      REPLACE ALL OCCURRENCES OF `__n__` IN out WITH `#`.
      REPLACE ALL OCCURRENCES OF `__d__` IN out WITH `$`.
      REPLACE ALL OCCURRENCES OF `__p__` IN out WITH `%`.
      REPLACE ALL OCCURRENCES OF `__m__` IN out WITH `&`.
      REPLACE ALL OCCURRENCES OF `__s__` IN out WITH `*`.
      REPLACE ALL OCCURRENCES OF `__h__` IN out WITH `-`.
      REPLACE ALL OCCURRENCES OF `__t__` IN out WITH `~`.
      REPLACE ALL OCCURRENCES OF `__l__` IN out WITH `/`.
      REPLACE ALL OCCURRENCES OF `__c__` IN out WITH `:`.
      REPLACE ALL OCCURRENCES OF `__v__` IN out WITH `|`.
      REPLACE ALL OCCURRENCES OF `__a__` IN out WITH `@`.
      REPLACE ALL OCCURRENCES OF `__o__` IN out WITH `.`.
      REPLACE ALL OCCURRENCES OF `___`   IN out WITH `.`.

      REPLACE ALL OCCURRENCES OF `__` IN out WITH `"`.

      SPLIT out AT `_` INTO TABLE tokens.
      LOOP AT tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF `"` IN out WITH `_`.

      cache-abap  = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
