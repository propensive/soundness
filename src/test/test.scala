package panopticon

import probably.*
import gossamer.*
import larceny.*

case class Organization(name: String, leader: Person)
case class Person(name: String, age: Int, role: Role)
case class Role(name: String, salary: Int)

object Tests extends Suite(t"Panopticon tests"):
  def run(): Unit =
    test(t"Check that correct type is inferred"):
      val salary = Lens[Organization](_.leader.role.salary)
      salary: Lens[Organization, ("salary", "role", "leader"), Int]
    .assert()

    test(t"Check that non-existant fields are inaccessible"):
      captureCompileErrors:
        Lens[Organization](_.age)
      .map(_.message)
    .assert(_ == List("panopticon: the field age is not a member of panopticon.Organization"))
    
    test(t"Check that indirect non-existant fields are inaccessible"):
      captureCompileErrors:
        Lens[Organization](_.leader.size)
      .map(_.message)
    .assert(_ == List("panopticon: the field size is not a member of panopticon.Person"))

    test(t"Check that two compatible lenses can be added"):
      val orgLeader = Lens[Organization](_.leader)
      val personName = Lens[Person](_.name)
      orgLeader ++ personName
    .assert()
    
    test(t"Check that two incompatible lenses can be added"):
      captureCompileErrors:
        val orgLeader = Lens[Organization](_.leader)
        val roleName = Lens[Role](_.name)
        orgLeader ++ roleName
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))


    val ceo = Role("CEO", 120000)
    val leader = Person("Jack Smith", 59, ceo)
    val org = Organization("Acme Inc", leader)

    test(t"Can apply a simple lens to get a value"):
      val lens = Lens[Organization](_.leader)
      lens.get(org)
    .assert(_ == leader)
    
    test(t"Can apply a lens to get a value"):
      val lens = Lens[Organization](_.leader.role.salary)
      lens.get(org)
    .assert(_ == 120000)
    
    test(t"Can updatea value with a simple lens"):
      val lens = Lens[Role](_.salary)
      val newRole: Role = lens.set(ceo, 100)
      newRole.salary
    .assert(_ == 100)
    
    test(t"Can updatea value with a deep lens"):
      val lens = Lens[Organization](_.leader.role.salary)
      val newOrganization: Organization = lens.set(org, 1000)
      newOrganization.leader.role.salary
    .assert(_ == 1000)


    // val orgName = new Lens[Organization, "name" *: EmptyTuple, String](_.name, (org, name) => org.copy(name = name))
    
    // test(t"Manual lens can access field"):
    //   orgName(org)
    // .assert(_ == "Acme Inc")
    
    // test(t"Manual lens can update field"):
    //   orgName(org) = "Emca Inc"
    // .assert(_.name == "Emca Inc")


