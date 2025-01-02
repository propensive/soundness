/*
    Revolution, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package revolution

package manifestAttributes:
  object ManifestVersion       extends ManifestAttribute["Manifest-Version"]()
  object MainClass             extends ManifestAttribute["Main-Class"]()
  object CreatedBy             extends ManifestAttribute["Created-By"]()
  object ClassPath             extends ManifestAttribute["Class-Path"]()
  object ContentType           extends ManifestAttribute["Content-Type"]()
  object ExtensionList         extends ManifestAttribute["Extension-List"]()
  object ExtensionName         extends ManifestAttribute["Extension-Name"]()
  object ImplementationTitle   extends ManifestAttribute["Implementation-Title"]()
  object ImplementationVendor  extends ManifestAttribute["Implementation-Vendor"]()
  object ImplementationVersion extends ManifestAttribute["Implementation-Version"]()
  object Sealed                extends ManifestAttribute["Sealed"]()
  object SignatureVersion      extends ManifestAttribute["Signature-Version"]()
  object SpecificationTitle    extends ManifestAttribute["Specification-Title"]()
  object SpecifacationVendor   extends ManifestAttribute["Specification-Vendor"]()
  object SpecifacationVersion  extends ManifestAttribute["Specification-Version"]()
