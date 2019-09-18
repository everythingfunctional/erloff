module Module_m
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: Module_t
        private
        type(VARYING_STRING) :: name
    contains
        private
        procedure :: moduleEquals
        generic, public :: operator(==) => moduleEquals
        procedure, public :: toString
        procedure, public :: repr
    end type Module_t

    interface Module_
        module procedure ModuleC
        module procedure ModuleS
    end interface Module_

    public :: Module_
contains
    pure function ModuleC(name)
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: name
        type(Module_t), pointer :: ModuleC

        allocate(ModuleC)
        ModuleC%name = name
    end function ModuleC

    pure function ModuleS(name)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: name
        type(Module_t), pointer :: ModuleS

        allocate(ModuleS)
        ModuleS%name = name
    end function ModuleS

    pure function moduleEquals(lhs, rhs)
        use iso_varying_string, only: operator(==)

        class(Module_t), intent(in) :: lhs
        type(Module_t), intent(in) :: rhs
        logical :: moduleEquals

        moduleEquals = lhs%name == rhs%name
    end function moduleEquals

    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Module_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%name
    end function toString

    pure function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Module_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = 'Module_t("' // self%name // '")'
    end function
end module Module_m
