------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                A S I S . D A T A _ D E C O M P O S I T I O N             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
--                                                                          --
-- This   specification  is  derived   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  The copyright --
-- notice above, and the license provisions that follow apply solely to the --
--  contents of the part following the private keyword.                     --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--  22   package Asis.Data_Decomposition (optional)
------------------------------------------------------------------------------
------------------------------------------------------------------------------
with Repinfo; use Repinfo;

package Asis.Data_Decomposition is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Data_Decomposition
--
--  This package is optional.
--
--  Operations to decompose data values using the ASIS type information and a
--  Portable_Data stream, representing a data value of that type.
--
--  An application can write data, using the
--  Asis.Data_Decomposition.Portable_Transfer package to an external medium
--  for later retrieval by another application.  The second application reads
--  that data and then uses this package to convert that data into useful
--  information.  Simple discrete scalar types can be converted directly into
--  useful information.  Composite types, such as records and arrays,
--  shall first be broken into their various discriminants and components.
--
--  A data stream representing a record value can be decomposed into a group
--  of discriminant and component data streams by extracting those streams
--  from the record's data stream.  This extraction is performed by applying
--  any of the Record_Components which describe the discriminants and
--  components of the record type.  Each discriminant and each component of a
--  record type is described by a Record_Component value.  Each value
--  encapsulates the information needed, by the implementation, to efficiently
--  extract the associated field from a data stream representing a record
--  value of the correct type.
--
--  A data stream representing an array value can be decomposed into a group
--  of component data streams by extracting those streams from the array's
--  data stream.  This extraction is performed by applying the single
--  Array_Component which describes the components of the array type.  One
--  Array_Component value is used to describe all array components.  The value
--  encapsulates the information needed, by the implementation, to efficiently
--  extract any of the array components.
--
--  Assumptions and Limitations of this Interface:
--
--  a) The data stream is appropriate for the ASIS host machine.  For example,
--     the implementation of this interface will not need to worry about
--     byte flipping or reordering of bits caused by movement of data between
--     machine architectures.
--
--  b) Records, arrays, and their components may be packed.
--
--  c) Records, array components, enumerations, and scalar types may have
--     representation and length clauses applied to them.  This includes scalar
--     types used as record discriminants and array indices.
--
--  d) This specification supports two of the three type models discussed
--     below.  Models A and B are supported.  Model C is not supported.
--
--     1) Simple "static" types contain no variants, have a single fixed 'Size,
--        and all components and attributes are themselves static and/or fully
--        constrained.  The size and position for any component of the type
--        can be determined without regard to constraints.  For example:
--
--           type Static_Record is
--               record
--                   F1, F2 : Natural;
--                   C1     : Wide_Character;
--                   A1     : Wide_String (1..5);
--               end record;
--
--           type Static_Discriminated (X : Boolean) is
--               record
--                   F1, F2 : Natural;
--                   C1     : Wide_Character;
--               end record;
--
--           type Static_Array   is array (Integer range 1 .. 100) of Boolean;
--           type Static_Enum    is (One, Two, Three);
--           type Static_Integer is range 1 .. 512;
--           type Static_Float   is digits 15 range -100.0 .. 100.0;
--           type Static_Fixed   is delta 0.1 range -100.0 .. 100.0;
--
--     2) Simple "dynamic" types contain one or more components or attributes
--        whose size, position, or value depends on the value of one or more
--        constraints computed at execution time.  This means that the size,
--        position, or number of components within the data type cannot be
--        determined without reference to constraint values.
--
--        Records containing components, whose size depends on discriminants
--        of the record, can be handled because the discriminants for a record
--        value are fully specified by the data stream form of the record
--        value. For example:
--
--            type Dynamic_Length (Length : Natural) is
--                record
--                    S1 : Wide_String (1 .. Length);
--                end record;
--
--            type Dynamic_Variant (When : Boolean) is
--                record
--                    case When is
--                        when True =>
--                            C1 : Wide_Character;
--                        when False =>
--                            null;
--                    end case;
--                end record;
--
--        Arrays with an unconstrained subtype, whose 'Length, 'First, and
--        'Last depend on dynamic index constraints, can be handled because
--        these attributes can be queried and stored when the data stream is
--        written. For example:
--
--             I : Integer := Some_Function;
--             type Dynamic_Array is
--                 array (Integer range I .. I + 10) of Boolean;
--
--             type Heap_Array   is array (Integer range <>) of Boolean;
--             type Access_Array is access Heap_Array;
--             X : Access_Array := new Heap_Array (1 .. 100);
--
--     3) Complex, externally "discriminated" records, contain one or more
--        components whose size or position depends on the value of one or
--        more non-static external values (values not stored within instances
--        of the type) at execution time.  The size for a value of the type
--        cannot be determined without reference to these external values,
--        whose runtime values are not known to the ASIS Context and cannot be
--        automatically recorded by the
--        Asis.Data_Decomposition.Portable_Transfer generics. For example:
--
--            N : Natural := Function_Call();
--            ....
--            declare
--                type Complex is
--                    record
--                        S1 : Wide_String (1 .. N);
--                    end record;
--            begin
--                ....
--            end;
--
--
--  General Usage Rules:
--
--  All operations in this package will attempt to detect the use of invalid
--  data streams.  A data stream is "invalid" if an operation determines that
--  the stream could not possibly represent a value of the expected variety.
--  Possible errors are: stream is of incorrect length, stream contains bit
--  patterns which are illegal, etc.  The exception ASIS_Inappropriate_Element
--  is raised in these cases.  The Status value is Data_Error.  The
--  Diagnosis string will indicate the kind of error detected.
--
--  All implementations will handle arrays with a minimum of 16 dimensions,
--  or the number of dimensions allowed by their compiler, whichever is
--  smaller.
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  22.1 type Record_Component
------------------------------------------------------------------------------
--  Record_Component
--
--  Describes one discriminant or component of a record type.  Implementation
--  is highly implementation dependent.  The "=" operator is not meaningful
--  between Record_Component values unless one of them is the
--  Nil_Record_Component value.
--
--  A record type describes composite values which contain zero or more
--  discriminant and component fields.  A_Record_Type_Definition can be
--  queried to obtain a list of Record_Components.  Each Record_Component
--  contains the information necessary to extract one discriminant or
--  component field of the record.
--
--  Record_Components are intended for use with data stream extraction
--  operations.  An extraction operation is performed using a Record_Component,
--  in conjunction with a data stream representing a value of the record type.
--  The record data stream contains data for all fields of the record.  The
--  result is an extracted data stream representing just the value of the one
--  field.  Record_Components are implemented so as to allow for efficient
--  extraction of field values.
--
--  An extracted field data stream is suitable for all uses.  If the field is a
--  scalar type, it can be converted directly into useful information.  If the
--  field is, in turn, another composite value, it can be further decomposed
--  into its own component values.
--
--  There are two ways to obtain the Record_Components or the Array_Component
--  needed to further decompose an embedded composite field.  First, if the
--  type of the field is known, the type definition can be directly queried to
--  obtain the Record_Components or the Array_Component that describe its
--  internal structure.  Second, the Record_Component used to extract the field
--  can be queried to obtain the same Record_Components or the same
--  Array_Component.  Both methods return identical information.
--
--  This kind of nested decomposition can be carried to any required level.
--
--  Record_Components become invalid when the Context, from which they
--  originate, is closed.  All Record_Components are obtained by referencing a)
--  an Element, which has an associated Context, b) another Record_Component,
--  or c) an Array_Component.  Ultimately, all component values originate from
--  a A_Type_Definition Element; that Element determines their Context of
--  origin.
------------------------------------------------------------------------------

   type Record_Component is private;
   Nil_Record_Component : constant Record_Component;

   function "=" (Left  : Record_Component;
                 Right : Record_Component)
                 return  Boolean is abstract;

------------------------------------------------------------------------------
--  22.2 type Record_Component_List
------------------------------------------------------------------------------

   type Record_Component_List is
      array (Asis.List_Index range <>) of Record_Component;

------------------------------------------------------------------------------
--  22.3 type Array_Component
------------------------------------------------------------------------------
--  Array_Component
--
--  Describes the components of an array valued field for a record type.
--  Implementation is highly implementor dependent.  The "=" operator is not
--  meaningful between Array_Component values unless one of them is the
--  Nil_Array_Component value.
--
--  An array type describes composite values which contain zero or more indexed
--  components. Both An_Unconstrained_Array_Definition or
--  A_Constrained_Array_Definition can be queried to obtain a single
--  Array_Component. The Array_Component contains the information necessary to
--  extract any arbitrary component of the array.
--
--  Array_Components are intended for use with data stream extraction
--  operations.  An extraction operation is performed using an Array_Component,
--  in conjunction with a data stream representing a value of the array type.
--  The array data stream contains data for all components of the array.  The
--  result is an extracted data stream representing just the value of the one
--  component.  Array_Components are implemented so as to allow for efficient
--  extraction of array component values.
--
--  An extracted component data stream is suitable for all uses.  If the
--  component is a scalar type, it can be converted directly into useful
--  information.  If the component is, in turn, another composite value, it can
--  be further decomposed into its own component values.
--
--  There are two ways to obtain the Record_Components or the Array_Component
--  needed to further decompose an embedded composite component.  First, if the
--  type of the component is known, the type definition can be directly queried
--  to obtain the Record_Components or the Array_Component that describe its
--  internal structure.  Second, the Array_Component used to extract the
--  component can be queried to obtain the same Record_Components or the same
--  Array_Component.  Both methods return identical information.
--
--  This kind of nested decomposition can be carried to any required level.
--
--  Array_Components become invalid when the Context, from which they
--  originate, is closed.  All Record_Components are obtained by referencing a)
--  an Element, which has an associated Context, b) a Record_Component, or c)
--  another Array_Component.  Ultimately, all component values originate from a
--  A_Type_Definition Element; that Element determines their Context of origin.
------------------------------------------------------------------------------

   type Array_Component is private;
   Nil_Array_Component : constant Array_Component;

   function "=" (Left  : Array_Component;
                 Right : Array_Component)
                 return  Boolean is abstract;

------------------------------------------------------------------------------
--  22.4 type Array_Component_List
------------------------------------------------------------------------------

   type Array_Component_List is
      array (Asis.List_Index range <>) of Array_Component;

------------------------------------------------------------------------------
--  22.5 type Dimension_Indexes
------------------------------------------------------------------------------
--  Dimension_Indexes - an array of index values used to access an array stream
------------------------------------------------------------------------------

   type Dimension_Indexes is
      array (Asis.ASIS_Positive range <>) of Asis.ASIS_Positive;

------------------------------------------------------------------------------
--  22.6 type Array_Component_Iterator
------------------------------------------------------------------------------
--  Array_Component_Iterator - Used to iterate over successive components of an
--  array.  This can be more efficient than using individual index values when
--  extracting array components from a data stream because it substitutes two
--  subroutine calls (Next and Done) for the multiplications and divisions
--  implicit in indexing an N dimensional array with a single index value.
--
--  Iterators can be copied.  The copies operate independently (have separate
--  state).
--
--  An example:
--
--   declare
--       Component        : Array_Component := ...;
--       Iter             : Array_Component_Iterator;
--       Array_Stream     : Portable_Data (...) := ...;
--       Component_Stream : Portable_Data (...);
--   begin
--       Iter := Array_Iterator (Component);
--       while not Done (Iter) loop
--           Component_Stream := Component_Data_Stream (Iter, Array_Stream);
--           Next (Iter);
--       end loop;
--   end;
------------------------------------------------------------------------------

   type Array_Component_Iterator is private;

   Nil_Array_Component_Iterator : constant Array_Component_Iterator;

------------------------------------------------------------------------------
--  22.7 type Portable_Data
------------------------------------------------------------------------------
--
--  Portable Data Representation - an ordered "stream" of data values
--
--  The portable representation for application data is an array of data
--  values.  This portable data representation is guaranteed to be valid when
--  written, and later read, on the same machine architecture, using the same
--  implementor's compiler and runtime system.  Portability of the data
--  values, across implementations and architectures, is not guaranteed.
--  Some implementors may be able to provide data values which are portable
--  across a larger subset of their supported machine architectures.
--
--  Some of the problems encountered when changing architectures are: bit
--  order, byte order, floating point representation, and alignment
--  constraints.  Some of the problems encountered when changing runtime
--  systems or implementations are: type representation, optimization,
--  record padding, and other I/O subsystem implementation variations.
--
--  The nature of these data values is deliberately unspecified.  An
--  implementor will choose a data value type that is suitable for the
--  expected uses of these arrays and data values.  Arrays and data
--  values have these uses:
--
--   a) Array values are used in conjunction with the
--      Asis.Data_Decomposition interface.  The data value type should be
--      readily decomposable, by that package, so that array and record
--      components can be efficiently extracted from a data stream represented
--      by this array type.  The efficiency of that interface is a priority.
--
--   b) The data value type is read and written by applications.  It
--      should have a size that makes efficient I/O possible.  Applications can
--      be expected to perform I/O in any or all of these ways:
--
--       1) Ada.Sequential_Io or Ada.Direct_Io could be used to read or write
--          these values.
--
--       2) Individual values may be placed inside other types and those types
--          may be read or written.
--
--       3) The 'Address of a data value, plus the 'Size of the data value
--          type, may be used to perform low level system I/O.  Note: This
--          requires the 'Size of the type and the 'Size of a variable of that
--          type to be the same for some implementations.
--
--       4) Individual values may be passed through Unchecked_Conversion in
--          order to obtain a different value type, of the same 'Size, suitable
--          for use with some user I/O facility.  This usage is non-portable
--          across implementations.
--
--   c) Array values are read and written by applications.  The data value
--      type should have a size that makes efficient I/O possible.
--      Applications can be expected to perform I/O in any or all of these
--      ways:
--
--       1) Ada.Sequential_Io or Ada.Direct_Io could be used to read or write a
--          constrained array subtype.
--
--       2) Array values may be placed inside other types and those types may
--          be read and written.
--
--       3) The 'Address of the first array value, plus the 'Length of the
--          array times the 'Size of the values, may be used to perform low
--          level system I/O.  Note: This implies that the array type is
--          unpacked, or, that the packed array type has no "padding" (e.g.,
--          groups of five 6-bit values packed into 32-bit words with 2 bits
--          of padding every 5 elements).
--
--       4) Array values may be passed through Unchecked_Conversion in order to
--          obtain an array value, with a different value type, suitable for
--          use with some user I/O facility.  This usage is non-portable across
--          implementations.
--
--  The data value type should be chosen so that the 'Address of the first
--  array data value is also the 'Address of the first storage unit containing
--  array data.  This is especially necessary for target architectures where
--  the "bit" instructions address bits in the opposite direction as that used
--  by normal machine memory (or array component) indexing.  A recommended
--  'Size is System.Storage_Unit (or a multiple of that size).
--
--  Implementations that do not support Unchecked_Conversion of array values,
--  or which do not guarantee that Unchecked_Conversion of array values will
--  always "do the right thing" (convert only the data, and not the dope vector
--  information), should provide warnings in their ASIS documentation that
--  detail possible consequences and work-arounds.
--
--  The index range for the Portable_Data type shall be a numeric type whose
--  range is large enough to encompass the Portable_Data representation for all
--  possible runtime data values.
--
--  All conversion interfaces always return Portable_Data array values with a
--  'First of one (1).
--
--  The Portable_Value type may be implemented in any way
--  whatsoever.  It need not be a numeric type.
------------------------------------------------------------------------------

   type Portable_Value is mod 2 ** 8;

   subtype Portable_Positive is Asis.ASIS_Positive
      range 1 .. Implementation_Defined_Integer_Constant;

   type Portable_Data is array (Portable_Positive range <>) of Portable_Value;

   Nil_Portable_Data : Portable_Data (1 .. 0);

------------------------------------------------------------------------------
--  22.8 type Type_Model_Kinds
------------------------------------------------------------------------------
--  Type_Model_Kinds
--
--  Each Type_Definition fits into one of three type models.
------------------------------------------------------------------------------

   type Type_Model_Kinds is (A_Simple_Static_Model,
                             A_Simple_Dynamic_Model,
                             A_Complex_Dynamic_Model,
                             Not_A_Type_Model);           -- Nil arguments

------------------------------------------------------------------------------
--  22.9 function Type_Model_Kind
------------------------------------------------------------------------------

   function Type_Model_Kind
     (Type_Definition : Asis.Type_Definition)
      return            Type_Model_Kinds;

   function Type_Model_Kind
     (Component : Record_Component)
      return      Type_Model_Kinds;

   function Type_Model_Kind
     (Component : Array_Component)
      return      Type_Model_Kinds;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the type definition to query
--  Component       - Specifies a record field with a record or array type
--
--  Returns the model that best describes the type indicated by the argument.
--  Returns Not_A_Type_Model for any unexpected argument such as a Nil value.
--
--  Expected Element_Kinds:
--      A_Type_Definition
--
--  --|A4G Type_Model_Kind is extended to operate on A_Subtype_Indication
--  --|A4G Elements
------------------------------------------------------------------------------
--  22.10        function Is_Nil
------------------------------------------------------------------------------

   function Is_Nil (Right : Record_Component) return Boolean;

   function Is_Nil (Right : Array_Component)  return Boolean;

------------------------------------------------------------------------------
--  Right   - Specifies the component to check
--
--  Returns True if Right is a Nil (or uninitialized) component value.
--
--  Returns False for all other values.
--
--  All component values are appropriate.
--
------------------------------------------------------------------------------
--  22.11        function Is_Equal
------------------------------------------------------------------------------

   function Is_Equal
     (Left  : Record_Component;
      Right : Record_Component)
      return  Boolean;

   function Is_Equal
     (Left  : Array_Component;
      Right : Array_Component)
      return  Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the left component to compare
--  Right   - Specifies the right component to compare
--
--  Returns True if Left and Right represent the same physical component of the
--  same record or array type, from the same physical compilation unit.  The
--  two components may or may not be from the same open ASIS Context variable.
--
--  Implies:
--    Is_Equal (Enclosing_Compilation_Unit (Component_Declaration (Left)),
--              Enclosing_Compilation_Unit (Component_Declaration (Right)))
--    = True
--
--  All component values are appropriate.
--
------------------------------------------------------------------------------
--  22.12        function Is_Identical
------------------------------------------------------------------------------

   function Is_Identical
     (Left  : Record_Component;
      Right : Record_Component)
      return  Boolean;

   function Is_Identical
     (Left  : Array_Component;
      Right : Array_Component)
      return  Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the left component to compare
--  Right   - Specifies the right component to compare
--
--  Returns True if Left and Right represent the same physical component of the
--  same record or array type, from the same physical compilation unit and the
--  same open ASIS Context variable.
--
--  Implies:
--    Is_Identical (Enclosing_Compilation_Unit (Component_Declaration (Left)),
--                  Enclosing_Compilation_Unit (Component_Declaration (Right)))
--    = True
--
--  All component values are appropriate.
--
------------------------------------------------------------------------------
--  22.13        function Is_Array
------------------------------------------------------------------------------

   function Is_Array (Component : Record_Component) return Boolean;

   function Is_Array (Component : Array_Component)  return Boolean;

------------------------------------------------------------------------------
--  Component   - Specifies any component
--
--  Returns True if the component has an array subtype (contains an array
--  value).
--
--  Returns False for Nil components and any component that is not an embedded
--  array.
--
------------------------------------------------------------------------------
--  22.14        function Is_Record
------------------------------------------------------------------------------

   function Is_Record (Component : Record_Component) return Boolean;
   function Is_Record (Component : Array_Component)  return Boolean;

------------------------------------------------------------------------------
--  Component   - Specifies any component
--
--  Returns True if the component has a record subtype.
--  Returns False for Nil components and any component that is not an embedded
--  record.
--

------------------------------------------------------------------------------
--  22.15        function Done
------------------------------------------------------------------------------

   function Done (Iterator : Array_Component_Iterator) return Boolean;

------------------------------------------------------------------------------
--  Iterator    - Specifies the iterator to query
--
--  Returns True if the iterator has been advanced past the last array
--  component.  Returns True for a Nil_Array_Component_Iterator.
--
------------------------------------------------------------------------------
--  22.16        procedure Next
------------------------------------------------------------------------------

   procedure Next (Iterator : in out Array_Component_Iterator);

------------------------------------------------------------------------------
--  Iterator    - Specifies the iterator to advance
--
--  Advances the iterator to the next array component.  Use Done to test the
--  iterator to see if it has passed the last component.  Does nothing if the
--  iterator is already past the last component.
--
------------------------------------------------------------------------------
--  22.17        procedure Reset
------------------------------------------------------------------------------

   procedure Reset (Iterator : in out Array_Component_Iterator);

------------------------------------------------------------------------------
--  Iterator    - Specifies the iterator to reset
--
--  Resets the iterator to the first array component.
--
------------------------------------------------------------------------------
--  22.18        function Array_Index
------------------------------------------------------------------------------

   function Array_Index
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Iterator    - Specifies the iterator to query
--
--  Returns the Index value which, when used in conjunction with the
--  Array_Component value used to create the Iterator, indexes the same array
--  component as that presently addressed by the Iterator.
--
--  Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
--  or one where Done(Iterator) = True.  The Status value is Data_Error.
--  The Diagnosis string will indicate the kind of error detected.
--
------------------------------------------------------------------------------
--  22.19        function Array_Indexes
------------------------------------------------------------------------------

   function Array_Indexes
     (Iterator : Array_Component_Iterator)
      return     Dimension_Indexes;

------------------------------------------------------------------------------
--  Iterator    - Specifies the iterator to query
--
--  Returns the index values which, when used in conjunction with the
--  Array_Component value used to create the Iterator, indexes the same array
--  component as that presently addressed by the Iterator.
--
--  Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
--  or one where Done(Iterator) = True.  The Status value is Data_Error.
--  The Diagnosis string will indicate the kind of error detected.
--
------------------------------------------------------------------------------
--  22.20        function Discriminant_Components
------------------------------------------------------------------------------
   function Discriminant_Components
     (Type_Definition : Asis.Type_Definition)
      return            Record_Component_List;

   function Discriminant_Components
     (Component : Record_Component)
      return      Record_Component_List;

   function Discriminant_Components
     (Component : Array_Component)
      return      Record_Component_List;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the record type definition to query
--  Component       - Specifies a component which has a record subtype,
--                      Is_Record(Component) = True
--
--  Returns a list of the discriminant components for records of the indicated
--  record type.
--
--  The result describes the locations of the record type's discriminants,
--  regardless of the static or dynamic nature of the record type.
--  All return components are intended for use with a data stream representing
--  a value of the indicated record type.
--
--  All Is_Record(Component) = True arguments are appropriate.  All return
--  values are valid parameters for all query operations.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from a record type)
--       A_Record_Type_Definition
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--       A_Simple_Dynamic_Model
--       A_Complex_Dynamic_Model
--
------------------------------------------------------------------------------
--  22.21        function Record_Components
------------------------------------------------------------------------------
   function Record_Components
     (Type_Definition : Asis.Type_Definition)
      return            Record_Component_List;

   function Record_Components
     (Component : Record_Component)
      return      Record_Component_List;

   function Record_Components
     (Component : Array_Component)
      return      Record_Component_List;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the record type definition to query
--  Component       - Specifies a component which has a record subtype,
--                      Is_Record(Component) = True
--
--  Returns a list of the discriminants and components for the indicated simple
--  static record type.  (See rule 6.A above.)
--
--  The result describes the locations of the record type's discriminants and
--  components.  All return components are intended for use with a data stream
--  representing a value of the indicated record type.
--
--  All Is_Record (Component) = True values, having simple static types, are
--  appropriate.  All return values are valid parameters for all query
--  operations.
--
--  Note: If an Ada implementation uses implementation-dependent record
--  components (Reference Manual 13.5.1 (15)), then each such component of
--  the record type is included in the result.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from a record type)
--       A_Record_Type_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--
------------------------------------------------------------------------------
--  22.22        function Record_Components
------------------------------------------------------------------------------

   function Record_Components
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data)
      return            Record_Component_List;

   function Record_Components
     (Component   : Record_Component;
      Data_Stream : Portable_Data)
      return        Record_Component_List;

   function Record_Components
     (Component   : Array_Component;
      Data_Stream : Portable_Data)
      return        Record_Component_List;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the record type definition to query
--  Component       - Specifies a component which has a record subtype,
--                      Is_Record(Component) = True
--  Data_Stream     - Specifies a data stream containing, at least, the
--                      complete set of discriminant or index constraints for
--                      the type
--
--  Returns a list of the discriminants and components for the indicated record
--  type, using the data stream argument as a guide.  The record type shall be
--  either a simple static, or a simple dynamic, record type.  (See rules 6.A
--  and 6.B above.)
--
--  The result describes the locations of the record type's discriminants and
--  all components of the appropriate variant parts.  The contents of the list
--  are determined by the discriminant values present in the data stream.
--
--  A simple static type will always return the same component list (Is_Equal
--  parts) regardless of the Data_Stream, because the layout of a simple static
--  type does not change with changes in discriminant values.  A simple dynamic
--  type returns different component lists (non-Is_Equal parts) depending on
--  the contents of the Data_Stream, because the contents and layout of a
--  simple dynamic type changes with changes in discriminant values.  All
--  return components are intended for use with a data stream representing a
--  value of the indicate record type.
--
--  The Data_Stream shall represent a fully discriminated value of the
--  indicated record type.  The stream may have been read from a file, it may
--  have been extracted from some enclosing data stream, or it may be an
--  artificial value created by the Construct_Artificial_Data_Stream operation.
--  Only the discriminant portion of the Data_Stream is checked for validity,
--  and, only some discriminant fields may need to be checked, depending on the
--  complexity of the record type.  The best approach, for any application that
--  is constructing artificial data streams, is to always provide appropriate
--  values for all discriminant fields.  It is not necessary to provide values
--  for non-discriminant fields.
--
--  All Is_Record(Component) = True values are appropriate.  All return values
--  are valid parameters for all query operations.
--
--  Note: If an Ada implementation uses implementation-dependent record
--  components (Reference Manual 13.5.1 (15)), then each such component of the
--  record type is included in the result.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from a record type)
--       A_Record_Type_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--       A_Simple_Dynamic_Model
--
------------------------------------------------------------------------------
--  22.23        function Array_Components
------------------------------------------------------------------------------

   function Array_Components
     (Type_Definition : Asis.Type_Definition)
      return            Array_Component;

   function Array_Components
     (Component : Record_Component)
      return      Array_Component;

   function Array_Components
     (Component : Array_Component)
      return      Array_Component;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the array type definition to query
--  Component       - Specifies a component which has an array subtype,
--                      Is_Array(Component) = True
--
--  Returns a single component, describing all components of the indicated
--  array type.  The array type shall be a simple static, or a simple dynamic
--  array type.  (See rules 6.A and 6.B above.)
--
--  The result contains all information necessary to index and extract any
--  component of a data stream representing a value of the indicated array
--  type.
--
--  All Is_Array (Component) = True values are appropriate.  All return values
--  are valid parameters for all query operations.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from an array type)
--       An_Unconstrained_Array_Definition
--       A_Constrained_Array_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--       A_Simple_Dynamic_Model
--
------------------------------------------------------------------------------
--  22.24        function Array_Iterator
------------------------------------------------------------------------------

   function Array_Iterator
     (Component : Array_Component)
      return      Array_Component_Iterator;

------------------------------------------------------------------------------
--  Component   - Specifies an array component to be used for iteration
--
--  Returns an iterator poised to fetch the 1st component of an array.
--

------------------------------------------------------------------------------
--  22.25        function Component_Data_Stream
------------------------------------------------------------------------------

   function Component_Data_Stream
     (Component   : Record_Component;
      Data_Stream : Portable_Data)
      return        Portable_Data;

   function Component_Data_Stream
     (Component   : Array_Component;
      Index       : Asis.ASIS_Positive;
      Data_Stream : Portable_Data)
      return        Portable_Data;

   function Component_Data_Stream
     (Component   : Array_Component;
      Indexes     : Dimension_Indexes;
      Data_Stream : Portable_Data)
      return        Portable_Data;

   function Component_Data_Stream
     (Iterator    : Array_Component_Iterator;
      Data_Stream : Portable_Data)
      return        Portable_Data;

------------------------------------------------------------------------------
--  Component   - Specifies the component or discriminant to be extracted
--  Index       - Specifies an index, 1..Array_Length, within an array
--  Indexes     - Specifies a list of index values, there is one value for
--                  each dimension of the array type, each index N is in the
--                  range 1..Array_Length (Component, N);
--  Iterator    - Specifies the array component to extract
--  Data_Stream - Specifies the data stream from which to extract the result
--
--  Returns a data stream representing just the value of the chosen Component.
--  The return value is sliced from the data stream.  The Data_Stream shall
--  represent a value of the appropriate type.  It may have been obtained from
--  a file, extracted from another data stream, or artificially constructed
--  using the Construct_Artificial_Data_Stream operation.
--
--  An artificial data stream may raise ASIS_Inappropriate_Element (the Status
--  is Value_Error).  Only the constraint values are valid, once they
--  have been properly initialized, and can be safely extracted from an
--  artificial data stream.
--
--  Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
--  or one where Done(Iterator) = True.  The Status value is Data_Error.
--  The Diagnosis string will indicate the kind of error detected.
--
--  All non-Nil component values are appropriate.
--
------------------------------------------------------------------------------
--  22.26        function Component_Declaration
------------------------------------------------------------------------------

   function Component_Declaration
     (Component : Record_Component)
      return      Asis.Declaration;

------------------------------------------------------------------------------
--  Component   - Specifies the component to be queried
--
--  Returns an Asis.Declaration, which is either A_Component_Declaration
--  or A_Discriminant_Specification.  These values can be used to determine the
--  subtype, type, and base type of the record component.  The result may be an
--  explicit declaration made by the user, or, it may be an implicit
--  component declaration for an implementation-defined component (Reference
--  Manual 13.5.1(15)).
--
--  All non-Nil component values are appropriate.
--
--  Returns Element_Kinds:
--       A_Declaration
--
--  Returns Declaration_Kinds:
--       A_Component_Declaration
--       A_Discriminant_Specification
--
------------------------------------------------------------------------------
--  22.27        function Component_Indication
------------------------------------------------------------------------------

   function Component_Indication
     (Component : Array_Component)
      return      Asis.Subtype_Indication;

------------------------------------------------------------------------------
--  Component   - Specifies the component to be queried
--
--  Returns an Asis.Subtype_Indication.  These values can be used to determine
--  the subtype, type, and base type of the array components.
--
--  All non-Nil component values are appropriate.
--
--  Returns Element_Kinds:
--       A_Subtype_Indication
--
------------------------------------------------------------------------------
--  22.28        function All_Named_Components
------------------------------------------------------------------------------

   function All_Named_Components
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Defining_Name_List;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the record type definition to query
--
--  Returns a list of all discriminant and component entity names defined by
--  the record type.  All record type definitions are appropriate for this
--  operation.  This query provides a means for determining whether a field,
--  with a particular name, exists for some possible instance of the record
--  type.  This list does not include the names of implementation-defined
--  components (Reference Manual 13.5.1 (15)); those name have the form of
--  An_Attribute_Reference expression.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from a record type)
--       A_Record_Type_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--       A_Simple_Dynamic_Model
--       A_Complex_Dynamic_Model
--
------------------------------------------------------------------------------
--  22.29        function Array_Length
------------------------------------------------------------------------------

   function Array_Length
     (Component : Record_Component)
      return      Asis.ASIS_Natural;

   function Array_Length
     (Component : Array_Component)
      return      Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Component   - Specifies the component to query
--
--  Returns the number of components within an array valued component.  The
--  array subtype may be multidimensional.  The result treats the array as if
--  it were unidimensional.  It is the product of the 'Lengths of the
--  individual array dimensions.
--
--  All Is_Array(Component) = True values are appropriate.
--
------------------------------------------------------------------------------
--  22.30        function Array_Length
------------------------------------------------------------------------------

   function Array_Length
     (Component : Record_Component;
      Dimension : Asis.ASIS_Natural)
      return      Asis.ASIS_Natural;

   function Array_Length
     (Component : Array_Component;
      Dimension : Asis.ASIS_Natural)
      return      Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Component   - Specifies the component to query
--  Dimension   - Specifies the array dimension to query
--
--  Returns the number of components within an array valued component.  The
--  array subtype may be unidimensional.  The result is the 'Length(Dimension)
--  of the array.
--
--  All Is_Array(Component) = True values are appropriate.
--
------------------------------------------------------------------------------
--  22.31        function Size
------------------------------------------------------------------------------

   function Size
     (Type_Definition : Asis.Type_Definition)
      return            Asis.ASIS_Natural;

   function Size
     (Component : Record_Component)
      return Asis.ASIS_Natural;

   function Size
     (Component : Array_Component)
      return Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Type_Definition - Specifies a type definition, whose 'Size is desired
--  Component       - Specifies a component, whose 'Size is desired
--
--  Returns the minimum number of bits required to hold a simple static type,
--  the number of bits allocated to hold a record field, or the number of bits
--  allocated to hold each array component.
--
--  --|AN Application Note:
--  --|AN
--  --|AN For components, this is the number of bits allocated
--  --|AN within the composite value.  It may be greater than the number
--  --|AN of bits occupied by data values of this component type.
--  --|AN Also, the data value, when occupying more space than is
--  --|AN minimally required, may be preceded, followed, or surrounded by
--  --|AN padding bits which are necessary to fully occupy the space allotted.
--  --|AN
--  All non-Nil component values are appropriate.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--
--  --|A4G Size is extended to operate on A_Subtype_Indication
--  --|A4G Elements
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--
------------------------------------------------------------------------------
--  22.32        function Size
------------------------------------------------------------------------------

   function Size
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data)
      return            Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the type definition to query
--  Data_Stream     - Specifies a data stream containing, at least, the
--                    complete set of discriminant or index constraints for
--                    the type
--
--  Returns the 'Size of a value of this type, with these constraints.  This is
--  the minimum number of bits that is needed to hold any possible value of the
--  given fully constrained subtype.  Only the constraint portion of the
--  Data_Stream is checked.
--
--  The Data_Stream may be a data stream or it may be an artificial
--  data stream created by the Construct_Artificial_Data_Stream operation.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Appropriate Asis.Data_Decomposition.Type_Model_Kinds:
--       A_Simple_Static_Model
--       A_Simple_Dynamic_Model
--
------------------------------------------------------------------------------
--  22.33        function Position
------------------------------------------------------------------------------

   function Position
     (Component : Record_Component)
      return      Asis.ASIS_Natural;

   function Position
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Natural;

   function Position
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Natural;

   function Position
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural;

------------------------------------------------------------------------------
--  Component   - Specifies the component to query
--  Index       - Specifies a value in the range 1..Array_Length (Component),
--                the index of the component to query
--  Indexes     - Specifies a list of index values, there is one value for
--                each dimension of the array type, each index N is in the
--                range 1..Array_Length (Component, N);
--  Iterator    - Specifies a particular array component to query
--
--  Returns the System.Storage_Unit offset, from the start of the first storage
--  unit occupied by the enclosing composite type, of the first of the storage
--  units occupied by the Component.  The offset is measured in storage units.
--
--  All non-Nil component values are appropriate.  Raises
--  ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
--  in the expected range or if Done (Iterator) = True.  The Status value will
--  be Data_Error.  The Diagnosis string will indicate the kind of error
--  detected.
--
------------------------------------------------------------------------------
--  22.34        function First_Bit
------------------------------------------------------------------------------

   function First_Bit
     (Component : Record_Component)
      return      Asis.ASIS_Natural;

   function First_Bit
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Natural;

   function First_Bit
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Natural;

   function First_Bit
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural;

------------------------------------------------------------------------------

--  Component   - Specifies the component to query
--  Index       - Specifies a value in the range 1..Array_Length (Component),
--                the index of the component to query
--  Indexes     - Specifies a list of index values, there is one value for
--                each dimension of the array type, each index N is in the
--                range 1..Array_Length (Component, N);
--  Iterator    - Specifies a particular array component to query
--
--  Returns the bit offset, from the start of the first of the storage units
--  occupied by the Component, of the first bit occupied by the Component.  The
--  offset is measured in bits.
--
--  All non-Nil component values are appropriate.  Raises
--  ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
--  in the expected range or if Done (Iterator) = True.  The Status value will
--  be Data_Error.  The Diagnosis string will indicate the kind of error
--  detected.
--
------------------------------------------------------------------------------
--  22.35        function Last_Bit
------------------------------------------------------------------------------

   function Last_Bit
     (Component : Record_Component)
      return      Asis.ASIS_Integer;

   function Last_Bit
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Integer;

   function Last_Bit
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Integer;

   function Last_Bit
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Integer;

------------------------------------------------------------------------------
--  Component   - Specifies the component to query
--  Index       - Specifies a value in the range 1..Array_Length (Component),
--                the index of the component to query
--  Indexes     - Specifies a list of index values, there is one value for
--                each dimension of the array type, each index N is in the
--                range 1..Array_Length (Component, N);
--  Iterator    - Specifies a particular array component to query
--
--  Returns the bit offset, from the start of the first of the storage units
--  occupied by the Index'th Element, of the last bit occupied by the Element.
--  The offset is measured in bits.
--
--  Note, that Last_Bit may be equal to -1 for a component which is
--  an empty array
--
--  All non-Nil component values are appropriate.  Raises
--  ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
--  in the expected range or if Done (Iterator) = True.  The Status value will
--  be Data_Error.  The Diagnosis string will indicate the kind of error
--  detected.
--
------------------------------------------------------------------------------
--  22.36        function Portable_Constrained_Subtype
------------------------------------------------------------------------------
--  Generic for Data Stream Conversions
------------------------------------------------------------------------------

   generic
      --  Ada notation for a constrained subtype.
      --  type Constrained_Subtype (<>) is private;
      type Constrained_Subtype is private;
   function Portable_Constrained_Subtype
     (Data_Stream : Portable_Data)
      return        Constrained_Subtype;

------------------------------------------------------------------------------
--  Data_Stream - Specifies an extracted component of a record
--
--  Instantiated with an appropriate scalar type, (e.g., System.Integer, can be
--  used to convert a data stream to a value that can be directly examined).
--
--  Instantiated with a record type, can be used to convert a data stream to a
--  value that can be directly examined.
--
--  Instantiations with constrained array subtypes may not convert array values
--  if they were created using the Portable_Array_Type_1,
--  Portable_Array_Type_2, or Portable_Array_Type_3 interfaces.
--
--  May raise Constraint_Error if the subtype is a scalar and the converted
--  value is not in the subtype's range.
--
------------------------------------------------------------------------------
--  22.37        function Construct_Artificial_Data_Stream
------------------------------------------------------------------------------

   function Construct_Artificial_Data_Stream
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data;
      Discriminant    : Record_Component;
      Value           : Portable_Data)
      return            Portable_Data;

------------------------------------------------------------------------------
--  Type_Definition - Specifies the record type definition for the record
--                    valued data stream being constructed
--  Data_Stream     - Specifies the data stream constructed so far; initially
--                    specified as the Nil_Portable_Data value
--  Discriminant    - Specifies the discriminant of the record type that is
--                    being set or changed
--  Value           - Specifies a data stream representing a single
--                    discriminant value of the appropriate type
--
--  Used to artificially construct a data stream which represents the
--  discriminant portion of a fully constrained value of the indicated record
--  type.  This operation is called once with a value for each discriminant of
--  the record type (the order in which the discriminants are specified is not
--  important).  The return value of each call is used as the input Data_Stream
--  for the next.
--
--  The resulting artificial data stream may be used solely for the purpose of
--  creating Record_Component values.  The values of any non-discriminant
--  fields are arbitrary and quite possibly invalid.  The resulting
--  component values may then be used for any purpose.  In particular, they may
--  be used to determine First_Bit, Last_Bit, and Size values for all record
--  discriminants and components.
--
--  Appropriate Element_Kinds:
--       A_Type_Definition
--
--  Appropriate Type_Kinds:
--       A_Derived_Type_Definition       (derived from a record type)
--       A_Record_Type_Definition
--
--
--  ??? Should be extended for A_Subtype_Indication
--
--  Raises ASIS_Inappropriate_Element, with a Status of Data_Error, if the
--  discriminant Value is inappropriate for the specified Discriminant.
--
------------------------------------------------------------------------------

private

   type Portable_Data_Access is access all Portable_Data;
   --  ??? Do we have to keep the whole parent stream? Is not a list
   --  ??? of parent discriminants (in the form suitable for A4G.DDA_Aux)
   --  ??? enough?

   type Discrim_List_Access is access all Discrim_List;

   type Record_Component is record

      Parent_Record_Type : Asis.Definition := Nil_Element;
      --  The definition of the type from which this component was extracted
      --  ??? Do we really need this?

      Component_Name : Asis.Defining_Name := Nil_Element;
      --  The defining name corresponding to the component in
      --  Parent_Record_Type
      --  ??? Why not just keep the entity node?

      Is_Record_Comp : Boolean := False;
      --  Flag indcating if the component itself is of a record type

      Is_Array_Comp : Boolean := False;
      --  Flag indcating if the component itself is of an array type

      Position : ASIS_Natural := 0;
      --  Position of the component, as it is to be returned by
      --  Position query

      First_Bit : ASIS_Natural := 0;
      --  Component first bit, as it is to be returned by First_Bit query

      Last_Bit : ASIS_Integer := 0;
      --  Component last bit, as it is to be returned by Last_Bit query
      --  Note, that Last_Bit may be equal to -1 for a component which is
      --  an empty array

      Size : ASIS_Natural := 0;
      --  Component size, as it is to be returned by Size query

      Parent_Discrims : Discrim_List_Access := null;
      --  The reference to a list of discriminants from the enclosed record
      --  value (null if there is no discriminant)

      Parent_Context : Context_Id := Non_Associated;
      --  An ASIS Context from which a given component is originated

      Obtained : ASIS_OS_Time := Nil_ASIS_OS_Time;
      --  Time when a given component was created, needed for validity
      --  checks

      --  What else do we need???

      --  ??? Do we need the reference to a tree file from which the
      --  ??? component is obtained (plus tree swapping mechanism similar
      --  ??? to what is used for Elements

   end record;

   Nil_Record_Component : constant Record_Component :=
     (Parent_Record_Type => Nil_Element,
      Component_Name     => Nil_Element,
      Is_Record_Comp     => False,
      Is_Array_Comp      => False,
      Position           => 0,
      First_Bit          => 0,
      Last_Bit           => 0,
      Size               => 0,
      Parent_Discrims    => null,
      Parent_Context     => Non_Associated,
      Obtained           => Nil_ASIS_OS_Time);

   type Dimention_Length is array (1 .. 16) of Asis.ASIS_Natural;

   type Array_Component is  record

      --  ??? Currently, the same structure as for Record_Component

      Parent_Array_Type : Asis.Definition := Nil_Element;
      --  The definition of the Array type to which this component
      --  belongs ???

      Array_Type_Entity : Entity_Id := Empty;
      --  Entyty Id for the array type from which the array component was
      --  extracted. It may not correspond to Parent_Array_Type, and it may
      --  be the implicit type entity as well.

      Parent_Component_Name : Asis.Defining_Name := Nil_Element;
      --  The defining name corresponding to the record component (which is of
      --  array type) to which a given array componnets directly belonds
      --  (Nil_Element, if the array component is not directly extracted from
      --  some record component). It may contain index constraint, so we
      --  need it. We also need it to compare array components.

      Is_Record_Comp : Boolean := False;
      --  Flag indcating if the component itself is of a record type

      Is_Array_Comp : Boolean := False;
      --  Flag indcating if the component itself is of an array type

      Position : ASIS_Natural := 0;
      --  Position of the component, as it is to be returned by
      --  Position query

      First_Bit : ASIS_Natural := 0;
      --  Component first bit, as it is to be returned by First_Bit query

      Last_Bit : ASIS_Integer := 0;
      --  Component last bit, as it is to be returned by Last_Bit query
      --  Note, that Last_Bit may be equal to -1 for a component which is
      --  an empty array

      Size : ASIS_Natural := 0;
      --  Component size, as it is to be returned by Size query

      Parent_Discrims : Discrim_List_Access := null;
      --  The reference to a list of discriminants from the enclosed record
      --  value (null if there is no discriminant), needed in case when the
      --  array componnet is extracted from a record component which in turn
      --  depends on discriminant

      Parent_Context : Context_Id := Non_Associated;
      --  An ASIS Context from which a given component is originated

      Obtained : ASIS_OS_Time := Nil_ASIS_OS_Time;
      --  Time when a given component was created, needed for validity
      --  checks

      --  What else do we need???

      Dimension : ASIS_Natural range 1 .. 16;
      --  Dimension of enclosing array value

      Length : Dimention_Length := (others => 0);

   end record;

   Nil_Array_Component : constant Array_Component :=
     (Parent_Array_Type     => Nil_Element,
      Array_Type_Entity     => Empty,
      Parent_Component_Name => Nil_Element,
      Is_Record_Comp        => False,
      Is_Array_Comp         => False,
      Position              => 0,
      First_Bit             => 0,
      Last_Bit              => 0,
      Size                  => 0,
      Parent_Discrims       => null,
      Parent_Context        => Non_Associated,
      Obtained              => Nil_ASIS_OS_Time,
      Dimension             => 1,
      Length                => (others => 0));

   type Array_Component_Iterator is
       record
           Component : Array_Component;
           Max_Len   : Asis.ASIS_Natural := 0;
           Index     : Asis.ASIS_Natural := 0;
       end record;

   Nil_Array_Component_Iterator : constant Array_Component_Iterator :=
      (Component => Nil_Array_Component,
       Max_Len   => 0,
       Index     => 0);

------------------------------------------------------------------------------

end Asis.Data_Decomposition;
