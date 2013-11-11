package rfp.recap

trait Simulation {
  protected type Action = () => Unit
  private type Agenda = List[Event]

  case class Event(time: Int, action: Action)

  private var agenda = List.empty[Event]

  private var curTime = 0

  protected def currentTime: Int = curTime

  protected def afterDelay(t: Int)(block: => Unit): Unit = insert(agenda, Event(currentTime + t, () => block))

  private def insert(a: Agenda, e: Event): Unit = {
      def loop(a: Agenda): Agenda = a match {
        case first :: rest if first.time < e.time => first :: loop(rest)
        case _                                    => e :: a
      }
    agenda = loop(a)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case _ =>
  }

  protected def run() = {
    afterDelay(0) { println(s"*** Starting simulation at time = '$curTime' ***") }
    loop()
  }
}

trait Gates {
  self: Simulation =>

  protected class Wire() {
    private var sigVal = false

    private var actions = List.empty[Action]

    def getSignal: Boolean = sigVal

    def setSignal(b: Boolean): Unit =
      if (sigVal != b) {
        sigVal = b
        actions foreach (_())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  protected def InverterDelay: Int

  protected def inverter(input: Wire, output: Wire): Unit = {
      def invertSignal(): Unit =
        afterDelay(InverterDelay) { output.setSignal(!input.getSignal) }

    input addAction invertSignal
  }

  private def letSignal(w: Wire)(f: Boolean => Unit) = f(w.getSignal)

  protected def AndGateDelay: Int

  protected def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
      def addSignals(): Unit =
        letSignal(a1) { sig1 =>
          letSignal(a2) { sig2 =>
            afterDelay(AndGateDelay) { output.setSignal(sig1 & sig2) }
          }
        }

    a1 addAction addSignals
    a2 addAction addSignals
  }

  protected def OrGateDelay: Int

  protected def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
      def orSignals(): Unit =
        letSignal(o1) { sig1 =>
          letSignal(o2) { sig2 =>
            afterDelay(OrGateDelay) { output.setSignal(sig1 | sig2) }
          }
        }

    o1 addAction orSignals
    o2 addAction orSignals
  }

  protected def probe(name: String, w: Wire): Unit = {
      def probeSignal(): Unit =
        afterDelay(0) { println(s"[$currentTime] [$name] new-value = ${w.getSignal}") }

    w addAction probeSignal
  }
}

trait Parameters {
  self: Gates =>
  override protected def InverterDelay = 2

  override protected def AndGateDelay = 3

  override protected def OrGateDelay = 5
}

trait Circuit extends Gates {
  self: Simulation =>
  protected def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {

    val d = new Wire
    val e = new Wire

    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  protected def fullAdder(a: Wire, b: Wire, cin: Wire, cout: Wire, sum: Wire) = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire

    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

trait SimulationContext extends Circuit with Simulation with Parameters

object TestSimulationContext extends SimulationContext {
  def main(args: Array[String]) {
    val in1, in2, sum, carry = new Wire
    halfAdder(in1, in2, sum, carry)

    probe("sum", sum)
    probe("carry", carry)

    in1 setSignal (true)
    run()

    in2 setSignal (true)
    run()

    in1 setSignal(false)
    run()
  }
}

