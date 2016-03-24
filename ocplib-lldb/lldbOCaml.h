/**************************************************************************/
/*                                                                        */
/*                             ocp-lldb                                   */
/*                                                                        */
/*  Copyright 2012-2015, OCamlPro                                         */
/*                                                                        */
/*  All rights reserved.  See accompanying files for the terms under      */
/*  which this file is distributed. In doubt, contact us at               */
/*  contact@ocamlpro.com (http://www.ocamlpro.com/)                       */
/*                                                                        */
/**************************************************************************/

#ifndef __LLDB_OCAML_H
#define __LLDB_OCAML_H


extern "C" {

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "caml/callback.h"
#include "caml/custom.h"

  /* wxClassID = the class of the pointer that we are storing */
extern value Val_abstract(const int class_id, const void *ptr);

  /* wxClassID = the class of the pointer that we are expecting to get */
extern void* Abstract_val(const int class_id, value ptr_v);
  value Val_final(const int class_id, const void* ptr);

  extern void* OCPLLDB_val(const int dest_id, value v);
  char **StringsOption_val(value strings_v);
  char *StringOption_val(value stringo_v);
}


#endif // __LLDB_OCAML_H

