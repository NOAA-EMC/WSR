      MODULE MACHINE

      IMPLICIT NONE
      SAVE
!  Machine dependant constants
      integer, parameter :: kind_io4  = 4, kind_io8  = 8
      integer, parameter :: kind_rad  = selected_real_kind(13,60) ! the '60' maps to 64-bit real
      integer, parameter :: kind_phys = selected_real_kind(13,60) ! the '60' maps to 64-bit real
      END MODULE MACHINE
