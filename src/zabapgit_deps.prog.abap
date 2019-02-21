REPORT zabapgit_deps.

PARAMETERS: p_git  TYPE text200 OBLIGATORY,
            p_devc TYPE devclass OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception.

  NEW zcl_abapgit_deps(
    iv_git_url = CONV #( p_git )
    iv_package = p_devc )->run( ).

ENDFORM.
