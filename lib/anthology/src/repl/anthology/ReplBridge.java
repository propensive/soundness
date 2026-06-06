/*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Soundness, version 0.54.0. © Copyright 2021-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
*/
package anthology;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Supplier;

// A process-global, session-keyed registry bridging runtime values from a host
// program's scope into separately-compiled REPL code (see `Repl.apply`). Written
// in Java deliberately: the surrounding Scala module is compiled with
// experimental language features, which would mark a Scala equivalent
// `@experimental` and make it unreferenceable from REPL code compiled without
// experimental mode. A Java class carries no such marker.
public final class ReplBridge {
  private ReplBridge() {}

  private static final ConcurrentHashMap<String, Object> registry = new ConcurrentHashMap<>();
  private static final AtomicLong counter = new AtomicLong(0L);
  private static final ThreadLocal<Long> current = new ThreadLocal<>();

  private static String key(long session, String name) { return session + " " + name; }

  public static long freshSession() { return counter.incrementAndGet(); }

  // The macro does not know the runtime session, so the seed object's accessors
  // call the session-less `fetchLive`/`updateLive` below, which read the session
  // from a thread-local set by the REPL immediately before each line runs. This
  // keeps the pickled seed block closed (its accessors refer only to `ReplBridge`,
  // never to a session value baked in at macro time).
  public static void setCurrentSession(long session) { current.set(session); }

  private static long currentSession() {
    Long session = current.get();
    return session == null ? 0L : session;
  }

  public static void put(long session, String name, Object value) {
    registry.put(key(session, name), value);
  }

  @SuppressWarnings("unchecked")
  public static <T> T fetch(long session, String name) {
    return (T) registry.get(key(session, name));
  }

  // A dynamic binding stores a supplier read live on each access, so a `def`
  // accessor in the REPL re-evaluates the host reference (e.g. a `var`) rather
  // than freezing its value at capture time.
  public static void putSupplier(long session, String name, Supplier<Object> supplier) {
    registry.put(key(session, name), supplier);
  }

  @SuppressWarnings("unchecked")
  public static <T> T fetchLive(long session, String name) {
    return (T) ((Supplier<Object>) registry.get(key(session, name))).get();
  }

  // Session-less variants used by seed accessors (session via thread-local).
  public static <T> T fetchLive(String name) { return fetchLive(currentSession(), name); }

  public static void updateLive(String name, Object value) {
    updateLive(currentSession(), name, value);
  }

  // A mutable dynamic binding also stores a consumer, so a `def name_=` accessor
  // in the REPL writes back to the host `var`.
  public static void putSetter(long session, String name, Consumer<Object> setter) {
    registry.put(key(session, name) + " set", setter);
  }

  @SuppressWarnings("unchecked")
  public static void updateLive(long session, String name, Object value) {
    ((Consumer<Object>) registry.get(key(session, name) + " set")).accept(value);
  }
}
