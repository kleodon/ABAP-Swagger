class ZCL_SWAG_MAP_TYPE definition
  public
  create public .

public section.

  class-methods GET_TYPEDESCR
    importing
      !IS_PARM type SEOSUBCODF
    returning
      value(RO_TYPEDESCR) type ref to CL_ABAP_TYPEDESCR .
  methods MAP
    returning
      value(RV_TYPE) type STRING .
  methods CONSTRUCTOR
    importing
      !IS_PARAM type SEOSUBCODF
      !IV_SCHEMA type ABAP_BOOL default ABAP_TRUE .
protected section.

  data MV_SCHEMA type ABAP_BOOL .
  data MS_PARAM type SEOSUBCODF .

  methods MAP_ELEMENT
    importing
      !IO_TYPEDESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(RV_TYPE) type STRING .
  methods MAP_INTERNAL
    importing
      !IO_TYPEDESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(RV_TYPE) type STRING .
  methods MAP_STRUCTURE
    importing
      !IO_TYPEDESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(RV_TYPE) type STRING .
  methods MAP_TABLE
    importing
      !IO_TYPEDESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(RV_TYPE) type STRING .
private section.
ENDCLASS.



CLASS ZCL_SWAG_MAP_TYPE IMPLEMENTATION.


  METHOD constructor.

    ms_param  = is_param.
    mv_schema = iv_schema.

  ENDMETHOD.


  METHOD get_typedescr.

    DATA: lv_name TYPE string.


    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = is_parm-type
      RECEIVING
        p_descr_ref    = ro_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
* try looking in the class
      ASSERT NOT is_parm-clsname IS INITIAL.
      CONCATENATE
        '\CLASS=' is_parm-clsname
        '\TYPE=' is_parm-type
        INTO lv_name.
      ro_typedescr = cl_abap_typedescr=>describe_by_name( lv_name ).
    ENDIF.

  ENDMETHOD.                    "get_typedescr


  METHOD map.

    rv_type = map_internal( get_typedescr( ms_param ) ).

  ENDMETHOD.


  METHOD map_element.

    DATA: lo_elemdescr TYPE REF TO cl_abap_elemdescr.

    lo_elemdescr ?= io_typedescr.

    CASE lo_elemdescr->type_kind.
      WHEN cl_abap_elemdescr=>typekind_string
          OR cl_abap_elemdescr=>typekind_date
          OR cl_abap_elemdescr=>typekind_time
          OR cl_abap_elemdescr=>typekind_num
          OR cl_abap_elemdescr=>typekind_hex.
        rv_type = '"type":"string"'.
      WHEN cl_abap_elemdescr=>typekind_char.
        rv_type = |"type":"string", "maxLength": { lo_elemdescr->output_length }|.
      WHEN cl_abap_elemdescr=>typekind_int1
          OR cl_abap_elemdescr=>typekind_int.
        rv_type = '"type":"integer"'.
      WHEN cl_abap_elemdescr=>typekind_packed.
        rv_type = '"type":"number"'.
      WHEN cl_abap_elemdescr=>typekind_xstring.
        rv_type = '"type":"string", "format": "binary"'.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD map_internal.

    CASE io_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_type = map_element( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_struct.
        rv_type = map_structure( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_table.
        rv_type = map_table( io_typedescr ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.                    "map_internal


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
      rv_type = rv_type && '"' && <ls_component>-name && '":{ ' && lv_type && ' }'.

      IF lv_index <> lines( lt_components ).
        rv_type = rv_type && ','.
      ENDIF.
    ENDLOOP.

    rv_type = rv_type && '}'.

    IF mv_schema = abap_true.
      rv_type = rv_type && '}'.
    ENDIF.

  ENDMETHOD.                    "map_structure


  METHOD map_table.

    DATA: lv_type  TYPE string,
          lo_table TYPE REF TO cl_abap_tabledescr.


    lo_table ?= io_typedescr.
    lv_type = map_internal( lo_table->get_table_line_type( ) ).

    IF mv_schema = abap_true.
      rv_type = '"schema":{"type":"array", "items":{' && lv_type && '}}'.
    ELSE.
      rv_type = '"type":"array", "items":{' && lv_type && '}'.
    ENDIF.

  ENDMETHOD.                    "map_table
ENDCLASS.
