package dice

import soundness.*

import android.app.Activity
import android.os.{Bundle, Handler, Looper, VibrationEffect, Vibrator}
import android.view.{Gravity, View}
import android.widget.{Button, LinearLayout, TextView, Toast}

// The Dice Roller from Google's "Android Basics" codelab, grown up a little: two dice, an
// animated roll, haptics, running statistics and a roll history — built programmatically (no
// XML layouts). Android's APIs are reached through xenophile's Kotlin facades — every call
// materializes as a direct JVM call, typechecked at compile time against the platform — while
// the rolls come from Kotlin's own `kotlin.random.Random` companion, the die colors are
// computed by iridescence (a golden-angle walk around the HSV hue circle), and the history
// line is joined by gossamer: Soundness libraries running on ART, straight out of the dexer.
class DiceActivity extends Activity:
  override def onCreate(saved: Bundle): Unit =
    super.onCreate(saved)

    // The activity itself joins facade-land (at its platform type: `DiceActivity` is still
    // being compiled, so only `Activity` is resolvable), and then even the final handoff to
    // the platform — `setContentView` — is a facade call, and facades never need unwrapping.
    val activity = Facade(this: Activity)
    val faces = IArray(t"⚀", t"⚁", t"⚂", t"⚃", t"⚄", t"⚅")

    var rolls = 0
    var total = 0
    var history: List[Text] = Nil

    val random = companion[kotlin.random.Random]
    val vibrator = Facade(getSystemService(classOf[Vibrator]))
    val handler = Kotlin.make[Handler](Looper.getMainLooper)

    def textView(size: Float): Facade over TextView =
      val view = Kotlin.make[TextView](this)
      view.setTextSize(size)
      view.setGravity(Gravity.CENTER_HORIZONTAL)
      view

    val left = textView(110.0f)
    val right = textView(110.0f)
    val stats = textView(18.0f)
    val historyView = textView(24.0f)

    left.setText(faces(0))
    right.setText(faces(0))
    stats.setText(t"Tap ROLL to begin")

    val button = Kotlin.make[Button](this)
    button.setText(t"Roll")

    // Each roll advances the hue by the golden angle; iridescence converts HSV to sRGB, and
    // the platform packs the channels.
    def dieColor(roll: Int): Int =
      val srgb = Hsv((roll*0.381966) % 1.0, 0.65, 0.75).to[Srgb]

      android.graphics.Color.rgb
        ((srgb.red*255).toInt, (srgb.green*255).toInt, (srgb.blue*255).toInt)

    def settle(): Unit =
      val first = random.nextInt(0, 6)
      val second = random.nextInt(0, 6)
      rolls += 1
      total += first + second + 2

      val color = dieColor(rolls)
      left.setText(faces(first))
      right.setText(faces(second))
      left.setTextColor(color)
      right.setTextColor(color)

      history = (t"${faces(first)}${faces(second)}" :: history).take(6)
      historyView.setText(history.join(t"  "))

      val mean = ((total.toDouble/rolls*100).toInt/100.0).toString.tt
      stats.setText(t"Rolls: $rolls   Total: $total   Mean: $mean")

      vibrator.vibrate(VibrationEffect.createOneShot(40L, VibrationEffect.DEFAULT_AMPLITUDE))

      if first == second then
        Toast.makeText(this, t"Doubles!".s, Toast.LENGTH_SHORT).show()

    def animate(frames: Int): Unit =
      if frames == 0 then settle() else
        left.setText(faces(random.nextInt(0, 6)))
        right.setText(faces(random.nextInt(0, 6)))

        // A lambda satisfies any Java functional-interface parameter: a nullary lambda is
        // always fully typed, so `Runnable` needs no ascription at all…
        handler.postDelayed(() => animate(frames - 1), 45L)

    // …and a unary lambda needs only its parameter's type (`Dynamic` erases the expected type,
    // so the parameter cannot be inferred), with no anonymous class or `override` in sight.
    button.setOnClickListener((_: View) => animate(6))

    val row = Kotlin.make[LinearLayout](this)
    row.setOrientation(LinearLayout.HORIZONTAL)
    row.setGravity(Gravity.CENTER)
    row.addView(left)
    row.addView(right)

    val layout = Kotlin.make[LinearLayout](this)
    layout.setOrientation(LinearLayout.VERTICAL)
    layout.setGravity(Gravity.CENTER)
    layout.addView(row)
    layout.addView(stats)
    layout.addView(historyView)
    layout.addView(button)

    activity.setContentView(layout)
