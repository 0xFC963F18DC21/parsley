/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import parsley.ParsleyTest

class XWeakMapSpec extends ParsleyTest {
    behavior of "XWeakMap"

    it should "allow lookup and removal of (strong) keys as a Map does" in {
        val xwm: XWeakMap[Object, Int] = new XWeakMap()

        // Keys are hardcoded here because otherwise they'd get GC-ed.
        val (k1, v1) = (new Object(), 0)
        val (k2, v2) = (new Object(), 1)

        xwm.put(k1, v1)
        xwm.put(k2, v2)

        xwm(k1) shouldBe 0
        xwm(k2) shouldBe 1

        info("it should also allow removal of keys")

        xwm.remove(k1)
        xwm.get(k1) shouldBe None
    }

    it should "allow replacement of values" in {
        val xwm: XWeakMap[Object, String] = new XWeakMap()
        val key = new Object()

        xwm.put(key, "foo")
        xwm.put(key, "bar")

        xwm(key) shouldBe "bar"
    }

    it should "resize while keeping all live entries" in {
        val xwm: XWeakMap[Object, Int] = new XWeakMap(32) // scalastyle:ignore magic.number
        val objs: Int = 256

        val keys: Array[Object] = Array.fill(objs)(new Object())
        for ((k, ix) <- keys.zipWithIndex) xwm.put(k, ix)
        for ((k, ix) <- keys.zipWithIndex) xwm(k) shouldBe ix

        xwm.backing.trueSize() shouldBe objs
        xwm.backing.trueSize() shouldBe xwm.backing.liveSize()
    }

    // TODO: How do I test for memory leaks (and coerce GC) in JS?
    alert("""There does not seem to be an easy way to encourage JS' GC to run. This test would've tested
            |the WeakRef's capability of not holding strong references to the keys of the map, but there
            |also is no way of doing so without creating an ungodly amount of objects, and I don't think
            |the CI runners would like that.""".stripMargin.replace("\r", "").replace('\n', ' '))
    ignore should "not leak memory when many short-lived objects exist" in {
        // I have no idea what to put in this test in general.

        // Set a relatively large number of objects to maximise time spent in the map, to maximise
        // the chances that GC gets triggered at least once.
        val objs: Int = 256000

        val xwm: XWeakMap[Object, Int] = new XWeakMap(objs / 16)
        for (i <- 0 until objs) {
            if (i % 1000 == 0) {
                // XXX: This is a horrible idea. System.gc() doesn't force garbage collection, merely
                //      "encourage" it to run.
                System.gc()
            }
            xwm.put(new Object(), i)
        }

        (xwm.backing.trueSize() < objs) shouldBe true
    }

    it should "not have an iterator at all" in {
        try {
            new XWeakMap[Object, Object]().iterator
            fail(".iterator somehow returned.")
        } catch {
            case _: Throwable => info(".iterator call has thrown, as expected")
        }
    }
}
