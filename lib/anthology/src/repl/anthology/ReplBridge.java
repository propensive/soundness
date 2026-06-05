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

  private static String key(long session, String name) { return session + " " + name; }

  public static long freshSession() { return counter.incrementAndGet(); }

  public static void put(long session, String name, Object value) {
    registry.put(key(session, name), value);
  }

  @SuppressWarnings("unchecked")
  public static <T> T fetch(long session, String name) {
    return (T) registry.get(key(session, name));
  }
}
