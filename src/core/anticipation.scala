package anticipation

import diuretic.*

package timeRepresentation:
  given javaTime: JavaTime.type = JavaTime
  given long: JavaLongTime.type = JavaLongTime
  given javaUtil: JavaUtilTime.type = JavaUtilTime

package fileRepresentation:
  given javaNio: JavaNioFile.type = JavaNioFile
  given javaIo: JavaIoFile.type = JavaIoFile

package urlRepresentation:
  given javaNet: JavaNetUrl.type = JavaNetUrl