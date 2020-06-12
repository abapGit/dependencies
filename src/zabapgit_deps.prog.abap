REPORT zabapgit_deps.

TABLES: sscrfields.

PARAMETERS: p_git  TYPE text200 OBLIGATORY,
            p_devc TYPE devclass OBLIGATORY,
            p_test TYPE c AS CHECKBOX.

INCLUDE zabapgit_password_dialog.
INCLUDE zabapgit_forms.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.

START-OF-SELECTION.
  PERFORM deps.

FORM deps.

  TRY.
      NEW zcl_abapgit_deps(
        iv_git_url = CONV #( p_git )
        iv_package = p_devc )->run( p_test ).
    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

ENDFORM.
