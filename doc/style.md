Soundness tutorials should be written in English prose, using American English spelling. Always write "runtime" and "compiletime" as single words, never as "run time", "run-time", "compile time" or "compile-time".

The flow should be paced appropriately for a listener who is being introduced to new concepts. A basic understanding of the subject matter can be presumed, though complex or esoteric details must be explained in detail. Anything requiring a moderate or complex explanation sholud be linked to external resources such as Wikipedia. Terms and phrases
should be limited to those which familiar to both British and American readers. In general, we need to explain Soundness implementations in depth, whereas we're not required to explain the concepts Soundness implements, but we should give enough of an explanation to explain the motivation behind Soundness's design.

Draw attention to typesafety and compile-time checking that's performed by Soundness, as well as drawing occasional attention to how these fit with Soundness's general design philosophy. Explain the design ideas behind the API, and draw particular attention to ideas which may be unconventional.

The text should be written in markdown. Code should be written in backticks. Specific names of terms and types should be written in backticks. Block-level code samples should be written in triple backticks, in idiomatic Soundness Scala, formatted according to the style described in syntax.md.

Code samples should be complete (including imports as necessary, but wildcard imports are fine), and should follow on from each other. That is, terms with the same name should mean the same thing, but there should not be multiple assignments to the same identifier.

The document structure should start with up to three introductory paragraphs giving an overview of the scope of the subqect matter, before going into detail on the different topics. Foundational details and the most commonly used features should be prioritized, while implementation details and esoteric features should be deprioritized. In the prose, refer to the project as "Soundness" rather than the internal module names.

The writing style should use Classic Style, as described by Steven Pinker. Classic Style is described as:

- Avoid first-person pronouns: I, me, my, mine
- Write as if you are showing the reader something in the world.
- The writer’s role is to point to something true or interesting and help the reader see it clearly.
- Treat writing as a conversation between equals.
- The reader is intelligent and capable, not a subordinate being lectured to.
- Assume truth exists and can be perceived clearly.
- The prose proceeds as if the subject can be directly understood and described.
- Aim for clarity above all else.
- Success is measured by whether the reader can effortlessly grasp the point.
- Use simple, direct phrasing.
- Prefer the straightforward expression over ornate, inflated, or overly technical wording.
- Project confidence.
- State things plainly rather than hedging excessively or apologizing for your claims.
- Hide the machinery of writing.
- Avoid drawing attention to the act of writing itself ("In this essay I will argue...", "It is
  important to note that...").
- Present, don’t announce.
- Deliver the idea itself rather than talking about your intention to deliver it.
- Minimize self-reference and meta-discussion.
- Avoid excessive references to yourself, the paper, the section, or the argument structure unless
  genuinely useful.
- Prefer concrete imagery and examples.
- Abstract ideas should often be grounded in tangible illustrations.
- Maintain an unobstructed style.
- Remove clutter, jargon, throat-clearing, and unnecessary qualifiers.
- Use elegant variation in rhythm and sentence structure.
- Classic style should feel natural and graceful, not robotic or monotonous.
- Revise until prose feels effortless.
- Good classic prose appears easy and natural, even if heavily edited.
- Sound like someone who understands the subject thoroughly.
- The voice should feel calm, knowledgeable, and unstrained.
- Guide the reader’s attention deliberately.
- Structure sentences so the reader naturally focuses on the important part. Pinker compares this to
  directing a viewer’s gaze.
- Avoid bureaucratic/formalistic prose
- Avoid academic hedging and overqualification
- Avoid excessive passive voice
- Avoid unnecessary jargon
- Avoid pretentious diction
- Avoid "Throat-clearing" introductions
- Avoid signposting every structural move ("Firstly...", "In conclusion...")
- Avoid rhetorical questions, and other devices that rely on context the reader may not share to be read as intended; state the point directly instead.
