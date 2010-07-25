class Program
	def initialize(*statements)
		@statements = statements
	end
	def to_s
		@statements.join("\n")
	end
end
