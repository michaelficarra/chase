class Terminal
	def initialize(identifier)
		@identifier = identifier
		@notted = false
	end
	def to_s
		(@notted ? '¬' : '') + @identifier
	end
	def not
		@notted = !@notted
	end
end

