package ambience

import exoskeleton.*

package environments:
  given daemonClient(using cli: Cli): Environment = cli.environment