with Interrupts; use Interrupts;

package Runtime_Support is
   procedure Exit_Handler with
     Export, Convention => C, External_Name => "__gnat_exit";
end Runtime_Support;
