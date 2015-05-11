class A {
  public boolean equals(Object o) { System.out.println("A.equals(Object o)"); return true; }
  public boolean equals(A a) { System.out.println("A.equals(A a)"); return true; }

  public static void f(Object o, A a) { System.out.println("f(Object, A)"); }
  public static void f(A a, Object o) { System.out.println("f(A, Object)"); }

  public static void main(String[] args) {
    Object oo = new Object();
    Object oa = new A();
    A aa = new A();

    System.out.println("oo.equals(oo)");
    oo.equals(oo); // Object.equals(Object)
    System.out.println("oo.equals(oa)");
    oo.equals(oa); // Object.equals(Object)
    System.out.println("oo.equals(aa)");
    oo.equals(aa); // Object.equals(Object)

    System.out.println("oa.equals(oo)");
    oa.equals(oo); // A.equals(Object)
    System.out.println("oa.equals(oa)");
    oa.equals(oa); // A.equals(Object)
    System.out.println("oa.equals(aa)");
    oa.equals(aa); // A.equals(Object)

    System.out.println("aa.equals(oo)");
    aa.equals(oo); // A.equals(Object)
    System.out.println("aa.equals(oa)");
    aa.equals(oa); // A.equals(Object)
    System.out.println("aa.equals(aa)");
    aa.equals(aa); // A.equals(A)

    A.f(aa, aa); // error: reference to f is ambiguous
  }
}
