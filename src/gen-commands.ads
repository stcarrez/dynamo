-----------------------------------------------------------------------
--  gen-commands -- Commands for dynamo
--  Copyright (C) 2011, 2012, 2017, 2018, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Util.Commands.Drivers;
with Util.Commands.Parsers;
with Util.Commands.Text_IO;
with Gen.Generator;
package Gen.Commands is

   package Drivers is
     new Util.Commands.Drivers (Context_Type  => Gen.Generator.Handler,
                                Config_Parser => Util.Commands.Parsers.No_Parser,
                                IO            => Util.Commands.Text_IO,
                                Driver_Name   => "gen-commands");

   subtype Command is Drivers.Command_Type;
   subtype Command_Access is Drivers.Command_Access;
   subtype Argument_List is Util.Commands.Argument_List;

   Driver : Drivers.Driver_Type;

   --  Print dynamo short usage.
   procedure Short_Help_Usage;

end Gen.Commands;
