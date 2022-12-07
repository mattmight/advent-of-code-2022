
# An abstract parent class for simulated file system objects:
class SimFileSystemObject 
  attr_accessor :parent
end


# A class for simulated directories:
class SimDirectory < SimFileSystemObject

  # Returns all subdirectories (including itself):
  def directories
    dirs = [self]

    @children.each do |name, object| 

      dirs = object.directories + dirs

    end

    return dirs
  end


  def isDirectory
    return true 
  end


  # Adds a child directory with the given name:
  def addDirectory(dirname)
    if @children.has_value?(dirname) 
      raise Exception.new "Trying to re-create a directory #{dirname}"
    end
    @children[dirname] = SimDirectory.new(self)
  end


  # Adds a file with the given name and size:
  def addFile(filename, filesize)
     if @children.has_value?(filename) 
      raise Exception.new "Trying to re-create a file #{filename}"
    end
    @children[filename] = SimFile.new(self, filesize)
  end

  
  # Returns the child file system object with this name: 
  def getChild(childName)
    return @children[childName] 
  end


  # Returns the size of this directory:
  def size

    # Check if we already cached the size:
    if @size != nil then return @size end

    total = 0

    # Sum over all children:
    @children.each do | name, object | 
      total +=  object.size
    end

    @size = total

    return total
  end


  def initialize(parent)
    @size = nil
    @parent = parent 
    @children = {} 
  end

end


# A class for simulated files:
class SimFile < SimFileSystemObject

  attr_accessor :size

  # Returns all subdirectories:
  def directories
    return []
  end

  def isDirectory
    return false
  end

  def initialize(parent,size)
    @parent = parent
    @size = size
  end

end


# Create a root directory for the simulated filesystem:
RootDir = SimDirectory.new(nil)


# A pointer to our current directory:
$CurrDir = RootDir


# Read the transcript, line by line:
File.readlines('input.txt').each do |line|

   # Tokenize the line:
   words = line.split

 
   # Check what kind of line this is: 
   #   Command, listing a directory, or listing a file?
   if (words[0] == "$")
     # We're in a command
     
     case words[1]
     when "cd"
       # Change directory command 
      
       target = words[2]

       if target == "/" 
         # Move to the root
         $CurrDir = RootDir

       elsif target == ".." 
         $CurrDir = $CurrDir.parent

       else
         $CurrDir = $CurrDir.getChild(target) 

       end

     when "ls"
       # It's a list files command -- nothing to do

     end # end case

   elsif (words[0] == "dir")

     # We're listing a directory
     dirname = words[1]

     # Create a child directory:
     $CurrDir.addDirectory(dirname) 
     

   else 
     # We're listing a file
     size = words[0].to_i
     filename = words[1]

     $CurrDir.addFile(filename, size)

   end # end if

end


puts "total size: #{RootDir.size}"


# Grab all directories in the filesystem:
AllDirectories = RootDir.directories

# Part 1:

$count = 0

# Add up all directories under the limit:
AllDirectories.each do |dir| 

  if dir.size <= 100000
    $count += dir.size
  end

end

puts "Part 1: #{$count}"




# Part 2
TotalSpace = 70000000
NeededSpace = 30000000
FreeSpace = TotalSpace - RootDir.size

# A pointer to the current smallest satisfying directory:
$Smallest = RootDir

# Find the smallest directory that satisfies the constraint:
AllDirectories.each do |dir|
  if ((FreeSpace + dir.size) >= NeededSpace) and (dir.size < $Smallest.size)
    $Smallest = dir
  end
end

puts "Part 2: #{$Smallest.size}"
