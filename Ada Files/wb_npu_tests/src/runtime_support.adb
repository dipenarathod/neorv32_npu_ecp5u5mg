-- Runtime support for bare-metal NEORV32
-- Provides missing runtime symbols similar to how demos handle it

package body Runtime_Support is
   procedure Exit_Handler is
   begin
      loop
         null;  -- Stay in an infinite loop
      end loop;
   end Exit_Handler;
end Runtime_Support; 
