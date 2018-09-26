package sandbox.functor

trait Printable[A] {
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        Printable.this.format(func(value))
    }
}

object Printable {
  def apply[A](implicit instance: Printable[A]) = instance
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)
}

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = 
    new Codec[B] {
      def encode(value: B): String = Codec.this.encode(enc(value))
      def decode(value: String): B = dec(Codec.this.decode(value))
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)
}