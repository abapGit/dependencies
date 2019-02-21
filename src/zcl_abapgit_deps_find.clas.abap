CLASS zcl_abapgit_deps_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS find
      IMPORTING
        !iv_package     TYPE devclass
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DEPS_FIND IMPLEMENTATION.


  METHOD find.

* todo

  ENDMETHOD.
ENDCLASS.
