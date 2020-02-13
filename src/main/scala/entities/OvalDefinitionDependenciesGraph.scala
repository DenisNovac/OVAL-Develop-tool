package entities

case class OvalDefinitionDependenciesGraph(rootDefinition: OvalDefinition,
                                           externalDefinitions: Vector[OvalDefinition],
                                           tests: Vector[OvalTest],
                                           objects: Vector[OvalObject],
                                           states: Vector[OvalState],
                                           variables: Vector[OvalVariable]
                                          )
