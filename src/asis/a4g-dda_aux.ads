------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           A 4 G . D D A _ A U X                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 14154 $
--                                                                          --
--           Copyright (C) 1999-2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains auxiliary routines used to support the data
--  decomposition annex. At the level of this package, types and components
--  are represented by their normal Entity_Id values. The corresponding
--  entities have the necessary representation information stored in them.

--  DDA_Aux acts as an interface between the main ASIS routines and all
--  representation issues.

with Asis.Data_Decomposition;

with Repinfo; use Repinfo;
with Types;   use Types;
with Uintp;   use Uintp;

package A4G.DDA_Aux is

   --------------------------------
   -- Portable_Data Declarations --
   --------------------------------

   --  Asis.Data_Decomposition.Portable_Value;

   --    Portable values are simply bytes, i.e. defined as mod 256. This is
   --    fine for all the byte addressable architectures that GNAT runs on
   --    now, and we will worry later about exotic cases (which may well
   --    never arise).

   --  Portable_Positive is Asis.Data_Decomposition.Portable_Positive;

   --    This is simply a synonym for Asis.ASIS_Positive

   --  Asis.Data_Decomposition.Portable_Data;

   --    This is array (Portable_Positive range <>) of Portable_Data. Thus
   --    it is simply an array of bytes. The representation of data in this
   --    format is as follows:
   --
   --      For non-scalar values, the data value is stored at the start of
   --      the value, occupying as many bits as needed. If there are padding
   --      bits (on the right), they are stored as zero bits when a value is
   --      retrieved, and ignored when a value is stored.
   --
   --      For scalar values, the data value is of length 1, 2, 4, or 8 bytes
   --      in proper little-endian or big-endian format with sign or zero
   --      bits filling the unused high order bits. For enumeration values,
   --      this is the Enum_Rep value, i.e. the actual stored value, not the
   --      Pos value. For biased types, the value is unsigned and biased.

   ---------------------------------
   -- Basic Interface Definiitons --
   ---------------------------------

   Variable_Rep_Info : exception;
   --  This exception is raised if an attempt is made to access representation
   --  information that depends on the value of a variable other than a
   --  discriminant for the current record. For example, if the length of
   --  a component of subtype String (1 .. N) is requested, and N is not a
   --  discriminant or a static constant.

   Invalid_Data : exception;
   --  Exception raised if an invalid data value is detected. There is no
   --  systematic checking for invalid values, but there are some situations
   --  in which bad values are detected, and this exception is raised.

   No_Component : exception;
   --  Exception raised if a request is made to access a component in a
   --  variant part of a record when the component does not exist for the
   --  particular set of discriminant values present. Also raised if a
   --  request is made to access an out of bounds subscript value for an
   --  array element.

   Null_Discrims : Repinfo.Discrim_List (1 .. 0) := (others => Uint_0);
   --  Used as default if no discriminants given

   ----------------------
   -- Utility Routines --
   ----------------------

   function Build_Discrim_List
     (Rec  : Entity_Id;
      Data : Asis.Data_Decomposition.Portable_Data)
      return Repinfo.Discrim_List;
   --  Given a record entity, and a value of this record type, builds a
   --  list of discriminants from the given value and returns it. The
   --  portable data value must include at least all the discriminants
   --  if the type is for a discriminated record. If Rec is other than
   --  a discriminated record type, then Data is ignored and Null_Discrims
   --  is returned.

   function Eval_Scalar_Node
     (N     : Node_Id;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Uint;
   --  This function is used to get the value of a node representing a value
   --  of a scalar type. Evaluation is possible for static expressions and
   --  for discriminant values (in the latter case, Discs must be provided
   --  and must contain the appropriate list of discriminants). Note that
   --  in the case of enumeration values, the result is the Pos value, not
   --  the Enum_Rep value for the given enumeration literal.
   --
   --    Raises Variable_Rep_Info is raised if the expression is other than
   --    a discriminant or a static expression, or if it is a discriminant
   --    and no discriminant list is provided.
   --
   --  Note that this functionality is related to that provided by
   --  Sem_Eval.Expr_Value, but this unit is not available in ASIS.

   function Linear_Index
     (Typ   : Entity_Id;
      Subs  : Asis.Data_Decomposition.Dimension_Indexes;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.ASIS_Natural;
   --  Given Typ, the entity for a definite array type or subtype, and Subs,
   --  a list of 1's origin subscripts (that is, as usual Dimension_Indexes
   --  has subscripts that are 1's origin with respect to the declared lower
   --  bound), this routine computes the corresponding zero-origin linear
   --  index from the start of the array data. The case of Fortran convention
   --  (with row major order) is properly handled.
   --
   --  Raises No_Component if any of the subscripts is out of range (i.e.
   --  exceeds the length of the corresponding subscript position).

   function UI_From_Aint (A : Asis.ASIS_Integer) return Uint;
   --  Converts ASIS_Integer value to Uint

   function UI_Is_In_Aint_Range (U : Uint) return Boolean;
   --  Determine if a universal integer value U is in range of ASIS_Integer.
   --  Returns True iff in range (meaning that UI_To_Aint can be safely used).

   function UI_To_Aint (U : Uint) return Asis.ASIS_Integer;
   --  Converts Uint value to ASIS_Integer, the result must be in range
   --  of ASIS_Integer, or otherwise the exception Invalid_Data is raised.

   --------------------------------
   -- Universal Integer Encoding --
   --------------------------------

   --  These routines deal with decoding and encoding scalar values from
   --  Uint to portable data format.

   function Encode_Scalar_Value
     (Typ  : Entity_Id;
      Val  : Uint)
      return Asis.Data_Decomposition.Portable_Data;
   --  Given Typ, the entity for a scalar type or subtype, this function
   --  constructs a portable data valuethat represents a value of this
   --  type given by Val. The value of the Uint may not exceed the largest
   --  scalar value supported by the implementation. The result will be 1,
   --  2, 4 or 8 bytes long depending on the value of the input, positioned
   --  to be intepreted as an integer, and sign or zero extended as needed.
   --  For enumeration types, the value is the Enum_Rep value, not the Pos
   --  value. For biased types, the bias is NOT present in the Uint value
   --  (part of the job of Encode_Scalar_Value is to introduce the bias).
   --
   --    Raises Invalid_Data if value is out of range of the base type of Typ

   function Encode_Scalar_Value
     (Typ  : Entity_Id;
      Val  : Asis.ASIS_Integer)
      return Asis.Data_Decomposition.Portable_Data;
   --  Similar to above function except input is ASIS_Integer instead of Uint

   function Decode_Scalar_Value
     (Typ  : Entity_Id;
      Data : Asis.Data_Decomposition.Portable_Data)
      return Uint;
   --  Given Typ, the entity for a scalar type or subtype, this function
   --  takes the portable data value Data, that represents a value of
   --  this type, and returns the equivalent Uint value. For enumeration
   --  types the value is the Enum_Rep value, not the Pos value. For biased
   --  types, the result is unbiased (part of the job of Decode_Scalar_Value
   --  is to remove the bias).

   --------------------------------
   -- Record Discriminant Access --
   --------------------------------

   function Extract_Discriminant
     (Data : Asis.Data_Decomposition.Portable_Data;
      Disc : Entity_Id)
      return Uint;
   --  This function can be used to extract a discriminant value from a
   --  record. Data is the portable data value representing the record
   --  value, and Disc is the E_Discriminant entity for the discriminant.

   function Set_Discriminant
     (Data : Asis.Data_Decomposition.Portable_Data;
      Disc : Entity_Id;
      Val  : Uint)
      return Asis.Data_Decomposition.Portable_Data;
   --  Given Data, a portable data value representing the prefix of a record
   --  value which may already have some other discriminant values set, this
   --  function creates a new Portable_Data value, increased in length
   --  if necessary, in which the discriminant represented by E_Discriminant
   --  entity Disc is set to the given value.
   --
   --    Raises Invalid_Data if the value does not fit in the field

   procedure Set_Discriminant
     (Data : in out Asis.Data_Decomposition.Portable_Data;
      Disc : Entity_Id;
      Val  : Uint);
   --  Similar to the function above, except that the modification is done
   --  in place on the given portable data value. In this case, the Data
   --  value must be long enough to contain the given discriminant field.

   function Set_Discriminant
     (Data : Asis.Data_Decomposition.Portable_Data;
      Disc : Entity_Id;
      Val  : Asis.ASIS_Integer)
      return Asis.Data_Decomposition.Portable_Data;
   --  Similar to function above, but takes argument in ASIS_Integer form

   procedure Set_Discriminant
     (Data : in out Asis.Data_Decomposition.Portable_Data;
      Disc : Entity_Id;
      Val  : Asis.ASIS_Integer);
   --  Similar to function above, but takes argument in ASIS_Integer form

   -----------------------------
   -- Record Component Access --
   -----------------------------

   function Component_Present
     (Comp  : Entity_Id;
      Discs : Repinfo.Discrim_List)
      return  Boolean;
   --  Determines if the given component is present or not. For the case
   --  where the component is part of a variant of a discriminated record,
   --  Discs must contain the full list of discriminants for the record,
   --  and the result is True or False depending on whether the variant
   --  containing the field is present or not for the given discriminant
   --  values. If the component is not part of a variant, including the
   --  case where the record is non-discriminated, then Discs is ignored
   --  and the result is always True.

   function Extract_Record_Component
     (Data  : Asis.Data_Decomposition.Portable_Data;
      Comp  : Entity_Id;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.Data_Decomposition.Portable_Data;
   --  Given Data, a portable data value representing a record value, this
   --  routine extracts the component value corresponding to E_Component
   --  entity Comp, and returns a new portable data value corresponding to
   --  this component. The Discs parameter supplies the discriminants and
   --  must be present in the discriminated record case if the component
   --  may depend on discriminants. For scalar types, the result value
   --  is 1,2,4, or 8 bytes, properly positioned to be interpreted as an
   --  integer, and sign/zero extended as required. For all other types,
   --  the value is extended if necessary to be an integral number of
   --  bytes by adding zero bits.
   --
   --    Raises No_Component if an attempt is made to set a component in a
   --    non-existent variant.
   --
   --    Raises No_Component if the specified component is part of a variant
   --    that does not exist for the given discriminant values
   --
   --    Raises Variable_Rep_Info if the size or position of the component
   --    depends on a variable other than a discriminant

   function Set_Record_Component
     (Data  : Asis.Data_Decomposition.Portable_Data;
      Comp  : Entity_Id;
      Val   : Asis.Data_Decomposition.Portable_Data;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.Data_Decomposition.Portable_Data;
   --  Given Data, a portable data value that represents a record value,
   --  or a prefix of such a record, sets the component represented by the
   --  E_Component entity Comp is set to the value represented by the portable
   --  data value Val, and the result is returned as a portable data value,
   --  extended in length if necessary to accomodate the newly added entry.
   --  The Discs parameter supplies the discriminants and must be present
   --  in the discriminated record case if the component may depend on
   --  the values of discriminants.
   --
   --    Raises No_Component if an attempt is made to set a component in a
   --    non-existent variant.
   --
   --    Raises No_Component if the specified component is part of a variant
   --    that does not exist for the given discriminant values
   --
   --    Raises Variable_Rep_Info if the size or position of the component
   --    depends on a variable other than a discriminant
   --
   --    Raises Invalid_Data if the data value is too large to fit.

   procedure Set_Record_Component
     (Data  : in out Asis.Data_Decomposition.Portable_Data;
      Comp  : Entity_Id;
      Val   : Asis.Data_Decomposition.Portable_Data;
      Discs : Repinfo.Discrim_List := Null_Discrims);
   --   Same as the above, but operates in place on the given data value,
   --   which must in this case be long enough to contain the component.

   ----------------------------
   -- Array Component Access --
   ----------------------------

   function Extract_Array_Component
     (Typ   : Entity_Id;
      Data  : Asis.Data_Decomposition.Portable_Data;
      Subs  : Asis.Data_Decomposition.Dimension_Indexes;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.Data_Decomposition.Portable_Data;
   --  Given Typ, the entity for an array type, and Data, a portable data
   --  value representing a value of this array type, this function extracts
   --  the array element corresponding to the given list of subscript values.
   --  The parameter Discs must be given if any of the bounds of the array
   --  may depend on discriminant values, and supplies the corresponding
   --  discriminants. For scalar component types, the value is 1,2,4, or 8,
   --  properly positioned to be interpreted as an integer, and sign/zero
   --  extended as required. For all other types, the value is extended if
   --  necessary to be an integral number of bytes by adding zero bits.
   --
   --    Raises No_Component if any of the subscripts is out of bounds
   --    (that is, exceeds the length of the corresponding index).
   --
   --    Raises Variable_Rep_Info if length of any index depends on a
   --    variable other than a discriminant.
   --
   function Set_Array_Component
     (Typ   : Entity_Id;
      Data  : Asis.Data_Decomposition.Portable_Data;
      Subs  : Asis.Data_Decomposition.Dimension_Indexes;
      Val   : Asis.Data_Decomposition.Portable_Data;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.Data_Decomposition.Portable_Data;
   --  Given a portable data value representing either a value of the array
   --  type Typ, or a prefix of such a value, sets the element referenced by
   --  the given subscript values in Subs, to the given value Val. The
   --  parameter Discs must be given if any of the bounds of the array
   --  may depend on discriminant values, and supplies the corresponding
   --  discriminants. The returned result is the original portable data
   --  value, extended if necessary to include the new element, with the
   --  new element value in place.
   --
   --    Raises No_Component if any of the subscripts is out of bounds
   --    (that is, exceeds the length of the corresponding index).
   --
   --    Raises Variable_Rep_Info if length of any index depends on a
   --    variable other than a discriminant.
   --
   --    Raises Invalid_Data if the data value is too large to fit.

   procedure Set_Array_Component
     (Typ   : Entity_Id;
      Data  : in out Asis.Data_Decomposition.Portable_Data;
      Subs  : Asis.Data_Decomposition.Dimension_Indexes;
      Val   : Asis.Data_Decomposition.Portable_Data;
      Discs : Repinfo.Discrim_List := Null_Discrims);
   --  Same as above, but operates in place on the given stream. The stream
   --  must be long enough to contain the given element in this case.

   -------------------------------
   -- Representation Parameters --
   -------------------------------

   --  These routines give direct access to the values of the
   --  representation fields stored in the tree, including the
   --  proper interpretation of variable values that depend on
   --  the values of discriminants.

   function Get_Esize
     (Comp  : Entity_Id;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Uint;
   --  Obtains the size (actually the object size, Esize) of any subtype
   --  or record component or discriminant. The function can be used for
   --  components of discriminanted or non-discriminated records. In the
   --  case of components of a discriminated record where the value depends
   --  on discriminants, Discs provides the necessary discriminant values.
   --  In all other cases, Discs is ignored and can be defaulted.
   --
   --    Raises Variable_Rep_Info if the size depends on a variable other
   --    than a discriminant.
   --
   --    Raises No_Component if the component is in a variant that does not
   --    exist for the given set of discriminant values.

   function Get_Component_Bit_Offset
     (Comp  : Entity_Id;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Uint;
   --  Obtains the component first bit value for the specified component or
   --  discriminant. This function can be used for discriminanted or non-
   --  discriminanted records. In the case of components of a discriminated
   --  record where the value depends on discriminants, Discs provides the
   --  necessary discriminant values. Otherwise (and in particular in the case
   --  of discriminants themselves), Discs is ignored and can be defaulted.
   --
   --    Raises Variable_Rep_Info if the size depends on a variable other
   --    than a discriminant.
   --
   --    Raises No_Component if the component is in a variant that does not
   --    exist for the given set of discriminant values.

   function Get_Component_Size
      (Typ  : Entity_Id)
       return Uint;
   --  Given an array type or subtype, returns the component size value

   function Get_Length
     (Typ   : Entity_Id;
      Sub   : Asis.ASIS_Positive;
      Discs : Repinfo.Discrim_List := Null_Discrims)
      return  Asis.ASIS_Natural;
   --  Given Typ, the entity for a definite array type or subtype, returns
   --  the Length of the subscript designated by Sub (1 = first subscript
   --  as in Length attribute). If the bounds of the array may depend on
   --  discriminants, then Discs contains the list of discriminant values
   --  (e.g. if we have an array field A.B, then the discriminants of A
   --  may be needed).
   --
   --    Raises Variable_Rep_Info if the size depends on a variable other
   --    than a discriminant.

   -------------------------------
   -- Computation of Attributes --
   -------------------------------

   --  The DDA_Aux package simply provides access to the representation
   --  information stored in the tree, as described in Einfo, as refined
   --  by the description in Repinfo with regard to the case where some
   --  of the values depend on discriminants.

   --  The ASIS spec requires that ASIS be able to compute the values of
   --  the attributes Position, First_Bit, and Last_Bit.

   --  This is done as follows. First compute the Size (with Get_Esize)
   --  and the Component_Bit_Offset (with Get_Component_Bit_Offset). In the
   --  case of a nested reference, e.g.

   --     A.B.C

   --       You need to extract the value A.B, and then ask for the
   --       size of its component C, and also the component first bit
   --       value of this component C.

   --       This value gets added to the Component_Bit_Offset value for
   --       the B field in A.

   --  For arrays, the equivalent of Component_Bit_Offset is computed simply
   --  as the zero origin linearized subscript multiplied by the value of
   --  Component_Size for the array. As another example, consider:

   --     A.B(15)

   --  In this case you get the component first bit of the field B in A,
   --  using the discriminants of A if there are any. Then you get the
   --  component first bit of the 15th element of B, using the discriminants
   --  of A (since the bounds of B may depend on them). You then add these
   --  two component first bit values.

   --  Once you have the aggregated value of the first bit offset (i.e. the
   --  sum of the Component_Bit_Offset values and corresponding array offsets
   --  for all levels of the access), then the formula is simply:

   --     X'Position = First_Bit_Offset / Storage_Unit_Size

   --     X'First    = First_Bit_Offset mod Storage_Unit_Size

   --     X'Last     = X'First + component_size - 1

end A4G.DDA_Aux;
