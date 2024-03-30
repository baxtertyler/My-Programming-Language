with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is

   type ExprC_Type is (NumC_Type, AppC_Type, IdC_Type, StrC_Type, LamC_Type);

   type NumC is record
      n : Integer;
   end record;

   type AppC is record
      func : Integer;
      arg : Integer;
   end record;

   type IdC is record
      func : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type StrC is record
      s : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type LamC is record
      arg : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type ExprC (Exp : ExprC_Type) is record
      case Exp is
         when NumC_Type =>
            Num : NumC;
         when AppC_Type =>
            App : AppC;
         when IdC_Type =>
            Id : IdC;
         when StrC_Type =>
            Str : StrC;
         when LamC_Type =>
            Lam : LamC;
      end case;
   end record;

   function Interp (exp : ExprC) return Integer is
   begin
      case exp.Exp is
         when NumC_Type =>
            return exp.Num.n;
         when AppC_Type =>
            return 0
         when IdC_Type =>
            return 0
         when StrC_Type =>
            return 0
         when LamC_Type =>
            return 0
         when others =>
            return -1;
      end case;
   end Interp;

   NumExpr : ExprC := (Exp => NumC_Type, Num => (n => 10));
   AppExpr : ExprC := (Exp => AppC_Type, App => (func => 5, arg => 3));
   IdExpr : ExprC := (Exp => IdC_Type, Id => (func => Ada.Strings.Unbounded.To_Unbounded_String("Hello")));
   StrExpr : ExprC := (Exp => StrC_Type, Str => (s => Ada.Strings.Unbounded.To_Unbounded_String("Hello")));
   LamExpr : ExprC := (Exp => LamC_Type, Lam => (arg => Ada.Strings.Unbounded.To_Unbounded_String("Hello")));

begin
   Ada.Text_IO.Put_Line(Integer'Image(Interp(NumExpr)));
   Ada.Text_IO.Put_Line(Integer'Image(Interp(AppExpr)));
   Ada.Text_IO.Put_Line(Integer'Image(Interp(IdExpr)));
   Ada.Text_IO.Put_Line(Integer'Image(Interp(StrExpr)));
   Ada.Text_IO.Put_Line(Integer'Image(Interp(LamExpr)));
end Main;
